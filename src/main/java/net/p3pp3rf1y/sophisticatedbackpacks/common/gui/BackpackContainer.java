package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.Lists;
import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.ints.IntComparators;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.CraftingResultSlot;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SSetSlotPacket;
import net.minecraft.util.NonNullList;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackUpgradeHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.NoopBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackBackgroundProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.ServerBackpackDataMessage;

import javax.annotation.Nullable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

public class BackpackContainer extends Container {
	public static final ResourceLocation EMPTY_UPGRADE_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_upgrade_slot");
	private static final int NUMBER_OF_PLAYER_SLOTS = 36;
	private static final String OPEN_TAB_ID_TAG = "openTabId";
	private static final String SORT_BY_TAG = "sortBy";
	private static final String UPGRADE_ENABLED_TAG = "upgradeEnabled";
	private static final String UPGRADE_SLOT_TAG = "upgradeSlot";
	private static final String ACTION_TAG = "action";

	private final IBackpackWrapper backpackWrapper;
	private final PlayerEntity player;
	private int backpackSlotNumber = -1;

	private final BackpackContext backpackContext;

	private final Map<Integer, UpgradeContainerBase<?, ?>> upgradeContainers = new LinkedHashMap<>();
	private Consumer<BackpackContainer> upgradeChangeListener = null;

	private final List<Slot> backpackInventorySlots = new ArrayList<>();

	private final BackpackBackgroundProperties backpackBackgroundProperties;

	public final NonNullList<ItemStack> upgradeItemStacks = NonNullList.create();
	public final List<Slot> upgradeSlots = Lists.newArrayList();

	private final IBackpackWrapper parentBackpackWrapper;

	private final Map<Integer, ItemStack> slotStacksToUpdate = new HashMap<>();

	private boolean isUpdatingFromPacket = false;

	public BackpackContainer(int windowId, PlayerEntity player, BackpackContext backpackContext) {
		super(backpackContext.getContainerType(), windowId);
		this.player = player;
		this.backpackContext = backpackContext;
		parentBackpackWrapper = backpackContext.getParentBackpackWrapper(player).orElse(NoopBackpackWrapper.INSTANCE);
		backpackWrapper = backpackContext.getBackpackWrapper(player);
		backpackWrapper.fillWithLoot(player);
		backpackBackgroundProperties = getNumberOfSlots() <= 81 ? BackpackBackgroundProperties.REGULAR : BackpackBackgroundProperties.WIDE;

		initSlotsAndContainers(player, backpackContext.getBackpackSlotIndex(), backpackContext.shouldLockBackpackSlot());
		backpackWrapper.getContentsUuid().ifPresent(backpackUuid ->
		{
			ItemStack backpack = backpackWrapper.getBackpack();
			BackpackAccessLogger.logPlayerAccess(player, backpack.getItem(), backpackUuid, backpack.getDisplayName().getString(),
					backpackWrapper.getClothColor(), backpackWrapper.getBorderColor());
		});

		backpackWrapper.getUpgradeHandler().runTemporaryBugFixToRemoveInvalidItems(player);
	}

	public IBackpackWrapper getParentBackpackWrapper() {
		return parentBackpackWrapper;
	}

	private void initSlotsAndContainers(PlayerEntity player, int backpackSlotIndex, boolean shouldLockBackpackSlot) {
		int yPosition = addBackpackInventorySlots();
		addPlayerInventorySlots(player.inventory, yPosition, backpackSlotIndex, shouldLockBackpackSlot);
		addBackpackUpgradeSlots(yPosition);
		addUpgradeSettingsContainers(player);
	}

	private void addUpgradeSettingsContainers(PlayerEntity player) {
		BackpackUpgradeHandler upgradeHandler = backpackWrapper.getUpgradeHandler();
		upgradeHandler.getSlotWrappers().forEach((slot, wrapper) -> UpgradeContainerRegistry.instantiateContainer(player, slot, wrapper)
				.ifPresent(container -> upgradeContainers.put(slot, container)));

		for (UpgradeContainerBase<?, ?> container : upgradeContainers.values()) {
			container.getSlots().forEach(this::addUpgradeSlot);
			container.onInit();
		}

		backpackWrapper.getOpenTabId().ifPresent(id -> {
			if (upgradeContainers.containsKey(id)) {
				upgradeContainers.get(id).setIsOpen(true);
			}
		});
	}

	private void addBackpackUpgradeSlots(int lastInventoryRowY) {
		BackpackUpgradeHandler upgradeHandler = backpackWrapper.getUpgradeHandler();

		int numberOfSlots = upgradeHandler.getSlots();

		if (numberOfSlots == 0) {
			return;
		}

		int slotIndex = 0;

		int yPosition = lastInventoryRowY - 22 * numberOfSlots;

		while (slotIndex < upgradeHandler.getSlots()) {
			addUpgradeSlot(new BackpackUpgradeSlot(upgradeHandler, slotIndex, yPosition));

			slotIndex++;
			yPosition += 22;
		}
	}

	protected void addUpgradeSlot(Slot slot) {
		slot.slotNumber = inventorySlots.size() + upgradeSlots.size();
		upgradeSlots.add(slot);
		upgradeItemStacks.add(ItemStack.EMPTY);
	}

	public void setUpgradeChangeListener(Consumer<BackpackContainer> upgradeChangeListener) {
		this.upgradeChangeListener = upgradeChangeListener;
	}

	private int addBackpackInventorySlots() {
		BackpackInventoryHandler inventoryHandler = backpackWrapper.getInventoryHandler();
		int slotIndex = 0;
		int yPosition = 18;

		while (slotIndex < inventoryHandler.getSlots()) {
			int lineIndex = slotIndex % getSlotsOnLine();
			int finalSlotIndex = slotIndex;
			backpackInventorySlots.add(addSlot(new BackpackInventorySlot(player.world.isRemote, backpackWrapper, inventoryHandler, finalSlotIndex, lineIndex, yPosition)));

			slotIndex++;
			if (slotIndex % getSlotsOnLine() == 0) {
				yPosition += 18;
			}
		}

		if (slotIndex % getSlotsOnLine() > 0) {
			yPosition += 18;
		}

		return yPosition;
	}

	private void addPlayerInventorySlots(PlayerInventory playerInventory, int yPosition, int backpackSlotIndex, boolean shouldLockBackpackSlot) {
		int playerInventoryXOffset = backpackBackgroundProperties.getPlayerInventoryXOffset();

		yPosition += 14;

		for (int i = 0; i < 3; ++i) {
			for (int j = 0; j < 9; ++j) {
				int slotIndex = j + i * 9 + 9;
				int xPosition = playerInventoryXOffset + 8 + j * 18;
				Slot slot = addBackpackSafeSlot(playerInventory, yPosition, slotIndex, xPosition, backpackSlotIndex, shouldLockBackpackSlot);
				addSlotAndUpdateBackpackSlotNumber(backpackSlotIndex, shouldLockBackpackSlot, slotIndex, slot);
			}
			yPosition += 18;
		}

		yPosition += 4;

		for (int slotIndex = 0; slotIndex < 9; ++slotIndex) {
			int xPosition = playerInventoryXOffset + 8 + slotIndex * 18;
			Slot slot = addBackpackSafeSlot(playerInventory, yPosition, slotIndex, xPosition, backpackSlotIndex, shouldLockBackpackSlot);
			addSlotAndUpdateBackpackSlotNumber(backpackSlotIndex, shouldLockBackpackSlot, slotIndex, slot);
		}
	}

	@Override
	public void setAll(List<ItemStack> items) {
		backpackWrapper.setPersistent(false);
		isUpdatingFromPacket = true;
		super.setAll(items);
		isUpdatingFromPacket = false;
		backpackWrapper.setPersistent(true);
		backpackWrapper.getInventoryHandler().saveInventory();
		backpackWrapper.getUpgradeHandler().saveInventory();
	}

	private Slot addBackpackSafeSlot(PlayerInventory playerInventory, int yPosition, int slotIndex, int xPosition, int backpackSlotIndex, boolean shouldLockBackpackSlot) {
		Slot slot;
		if (shouldLockBackpackSlot && slotIndex == backpackSlotIndex) {
			slot = new Slot(playerInventory, slotIndex, xPosition, yPosition) {
				@Override
				public boolean canTakeStack(PlayerEntity playerIn) {
					return false;
				}

				@Override
				public void onSlotChanged() {
					super.onSlotChanged();
					closeBackpackScreenIfSomethingMessedWithBackpackStack(getStack());
				}
			};
		} else {
			slot = new Slot(playerInventory, slotIndex, xPosition, yPosition);
		}

		return addSlot(slot);
	}

	public void closeBackpackScreenIfSomethingMessedWithBackpackStack(ItemStack supposedToBeBackpackStack) {
		if (!isClientSide() && isNotCorrectBackpack(supposedToBeBackpackStack)) {
			player.closeScreen();
		}
	}

	private boolean isNotCorrectBackpack(ItemStack supposedToBeBackpackStack) {
		return supposedToBeBackpackStack.isEmpty() || !(supposedToBeBackpackStack.getItem() instanceof BackpackItem) || supposedToBeBackpackStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(w -> w != (isFirstLevelBackpack() ? backpackWrapper : parentBackpackWrapper)).orElse(true);
	}

	private boolean isClientSide() {
		return player.world.isRemote;
	}

	private void addSlotAndUpdateBackpackSlotNumber(int backpackSlotIndex, boolean lockBackpackSlot, int slotIndex, Slot slot) {
		if (lockBackpackSlot && slotIndex == backpackSlotIndex) {
			backpackSlotNumber = slot.slotNumber;
		}
	}

	public int getNumberOfRows() {
		return (int) Math.ceil((double) getNumberOfSlots() / getSlotsOnLine());
	}

	private int getSlotsOnLine() {
		return backpackBackgroundProperties.getSlotsOnLine();
	}

	@Override
	public boolean canInteractWith(PlayerEntity player) {
		return backpackContext.canInteractWith(player);
	}

	public static BackpackContainer fromBufferItem(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, BackpackContext.Item.fromBuffer(packetBuffer));
	}

	public static BackpackContainer fromBufferBlock(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, BackpackContext.Block.fromBuffer(packetBuffer));
	}

	public static BackpackContainer fromBufferItemSubBackpack(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, BackpackContext.ItemSubBackpack.fromBuffer(packetBuffer));
	}

	public static BackpackContainer fromBufferBlockSubBackpack(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, BackpackContext.BlockSubBackpack.fromBuffer(packetBuffer));
	}

	@Override
	public ItemStack transferStackInSlot(PlayerEntity playerIn, int index) {
		ItemStack itemstack = ItemStack.EMPTY;
		Slot slot = getSlot(index);
		if (slot.getHasStack()) {
			Optional<UpgradeContainerBase<?, ?>> upgradeContainer = getSlotUpgradeContainer(slot);
			ItemStack slotStack = upgradeContainer.map(c -> c.getSlotStackToTransfer(slot)).orElse(slot.getStack());
			itemstack = slotStack.copy();

			if (!mergeSlotStack(slot, index, slotStack, true)) {
				return ItemStack.EMPTY;
			}

			if (slotStack.isEmpty()) {
				slot.putStack(ItemStack.EMPTY);
			} else {
				slot.onSlotChanged();
			}
			slot.onSlotChange(slotStack, itemstack);

			if (upgradeContainer.isPresent()) {
				upgradeContainer.ifPresent(c -> c.onTakeFromSlot(slot, player, slotStack));
			} else {
				slot.onTake(player, slotStack);
			}
		}

		return itemstack;
	}

	private boolean mergeSlotStack(Slot slot, int index, ItemStack slotStack, boolean transferMaxStackSizeFromSource) {
		if (isBackpackInventoryOrUpgradeSlot(index)) {
			if (Boolean.TRUE.equals(Config.COMMON.shiftClickIntoOpenTabFirst.get())) {
				return mergeStackToOpenUpgradeTab(slotStack, transferMaxStackSizeFromSource) || mergeStackToPlayersInventory(slotStack, transferMaxStackSizeFromSource);
			}
			return mergeStackToPlayersInventory(slotStack, transferMaxStackSizeFromSource) || mergeStackToOpenUpgradeTab(slotStack, transferMaxStackSizeFromSource);
		} else if (isUpgradeSettingsSlot(index)) {
			if (getSlotUpgradeContainer(slot).map(c -> c.mergeIntoBackpackFirst(slot)).orElse(true)) {
				return mergeStackToBackpack(slotStack) || mergeStackToPlayersInventory(slotStack, true);
			}
			return mergeStackToPlayersInventory(slotStack, true) || mergeStackToBackpack(slotStack);
		} else {
			if (Boolean.TRUE.equals(Config.COMMON.shiftClickIntoOpenTabFirst.get())) {
				return mergeStackToOpenUpgradeTab(slotStack, true) || mergeStackToUpgradeSlots(slotStack) || mergeStackToBackpack(slotStack);
			}
			return mergeStackToUpgradeSlots(slotStack) || mergeStackToBackpack(slotStack) || mergeStackToOpenUpgradeTab(slotStack, true);
		}
	}

	private boolean mergeStackToUpgradeSlots(ItemStack slotStack) {
		return !upgradeSlots.isEmpty() && mergeItemStack(slotStack, inventorySlots.size(), inventorySlots.size() + getNumberOfUpgradeSlots(), false);
	}

	private boolean mergeStackToOpenUpgradeTab(ItemStack slotStack, boolean transferMaxStackSizeFromSource) {
		return getOpenContainer().map(c -> {
			List<Slot> slots = c.getSlots();
			if (slots.isEmpty()) {
				return false;
			}
			int firstSlotIndex = slots.get(0).slotNumber;
			int lastSlotIndex = slots.get(slots.size() - 1).slotNumber;
			return mergeItemStack(slotStack, firstSlotIndex, lastSlotIndex + 1, false, transferMaxStackSizeFromSource);
		}).orElse(false);
	}

	private boolean mergeStackToBackpack(ItemStack slotStack) {
		return mergeItemStack(slotStack, 0, getNumberOfSlots(), false);
	}

	private boolean mergeStackToPlayersInventory(ItemStack slotStack, boolean transferMaxStackSizeFromSource) {
		return mergeItemStack(slotStack, getNumberOfSlots(), inventorySlots.size(), true, transferMaxStackSizeFromSource);
	}

	private boolean isUpgradeSettingsSlot(int index) {
		return index >= getNumberOfSlots() + getNumberOfUpgradeSlots() + NUMBER_OF_PLAYER_SLOTS;
	}

	private boolean isBackpackInventoryOrUpgradeSlot(int index) {
		return isBackpackInventorySlot(index) || isUpgradeSlot(index);
	}

	private boolean isBackpackInventorySlot(int index) {
		return index < getNumberOfSlots();
	}

	private boolean isUpgradeSlot(int index) {
		return index >= getFirstUpgradeSlot() && (index - getFirstUpgradeSlot() < getNumberOfUpgradeSlots());
	}

	private int getFirstUpgradeSlot() {
		return inventorySlots.size();
	}

	public Optional<UpgradeContainerBase<?, ?>> getSlotUpgradeContainer(Slot slot) {
		if (isUpgradeSettingsSlot(slot.slotNumber)) {
			for (UpgradeContainerBase<?, ?> upgradeContainer : upgradeContainers.values()) {
				if (upgradeContainer.containsSlot(slot)) {
					return Optional.of(upgradeContainer);
				}
			}
		}
		return Optional.empty();
	}

	@Override
	public ItemStack slotClick(int slotId, int dragType, ClickType clickType, PlayerEntity player) {
		if (isUpgradeSettingsSlot(slotId) && getSlot(slotId) instanceof IFilterSlot && getSlot(slotId).isItemValid(player.inventory.getItemStack())) {
			ItemStack cursorStack = player.inventory.getItemStack().copy();
			if (cursorStack.getCount() > 1) {
				cursorStack.setCount(1);
			}

			getSlot(slotId).putStack(cursorStack);
			return ItemStack.EMPTY;
		} else if (isUpgradeSlot(slotId) && getSlot(slotId) instanceof BackpackUpgradeSlot && getSlot(slotId).isItemValid(player.inventory.getItemStack())) {
			BackpackUpgradeSlot upgradeSlot = (BackpackUpgradeSlot) getSlot(slotId);
			ItemStack cursorStack = player.inventory.getItemStack();
			ItemStack slotStack = upgradeSlot.getStack();
			if (!slotStack.isEmpty() && !cursorStack.isEmpty() && upgradeSlot.canSwapStack(player, cursorStack)) {
				player.inventory.setItemStack(slotStack);
				upgradeSlot.putStack(cursorStack);
				upgradeSlot.onSlotChanged();
				return slotStack.copy();
			}
		}

		return super.slotClick(slotId, dragType, clickType, player);
	}

	public int getNumberOfSlots() {
		return backpackWrapper.getInventoryHandler().getSlots();
	}

	public BackpackBackgroundProperties getBackpackBackgroundProperties() {
		return backpackBackgroundProperties;
	}

	public int getNumberOfUpgradeSlots() {
		return backpackWrapper.getUpgradeHandler().getSlots();
	}

	public int getFirstUpgradeSettingsSlot() {
		return getNumberOfSlots() + getNumberOfUpgradeSlots() + NUMBER_OF_PLAYER_SLOTS;
	}

	public Map<Integer, UpgradeContainerBase<?, ?>> getUpgradeContainers() {
		return upgradeContainers;
	}

	public void handleMessage(CompoundNBT data) {
		if (data.contains("containerId")) {
			int containerId = data.getInt("containerId");
			if (upgradeContainers.containsKey(containerId)) {
				upgradeContainers.get(containerId).handleMessage(data);
			}
		} else if (data.contains(OPEN_TAB_ID_TAG)) {
			setOpenTabId(data.getInt(OPEN_TAB_ID_TAG));
		} else if (data.contains(SORT_BY_TAG)) {
			setSortBy(SortBy.fromName(data.getString(SORT_BY_TAG)));
		} else if (data.contains(ACTION_TAG) && data.getString(ACTION_TAG).equals("sort")) {
			sort();
		} else if (data.contains(UPGRADE_ENABLED_TAG)) {
			setUpgradeEnabled(data.getInt(UPGRADE_SLOT_TAG), data.getBoolean(UPGRADE_ENABLED_TAG));
		}
	}

	public List<Slot> getBackpackInventorySlots() {
		return backpackInventorySlots;
	}

	public void setOpenTabId(int tabId) {
		if (isClientSide()) {
			CompoundNBT data = new CompoundNBT();
			data.putInt(OPEN_TAB_ID_TAG, tabId);
			PacketHandler.sendToServer(new ServerBackpackDataMessage(data));
		}

		if (tabId == -1) {
			backpackWrapper.removeOpenTabId();
		} else {
			backpackWrapper.setOpenTabId(tabId);
		}
	}

	public void removeOpenTabId() {
		setOpenTabId(-1);
	}

	public SortBy getSortBy() {
		return backpackWrapper.getSortBy();
	}

	public void setSortBy(SortBy sortBy) {
		if (isClientSide()) {
			CompoundNBT data = new CompoundNBT();
			data.putString(SORT_BY_TAG, sortBy.getString());
			PacketHandler.sendToServer(new ServerBackpackDataMessage(data));
		}
		backpackWrapper.setSortBy(sortBy);
	}

	public void sort() {
		if (isClientSide()) {
			CompoundNBT data = new CompoundNBT();
			data.putString(ACTION_TAG, "sort");
			PacketHandler.sendToServer(new ServerBackpackDataMessage(data));
			return;
		}

		backpackWrapper.sort();
	}

	public boolean isFirstLevelBackpack() {
		return parentBackpackWrapper == NoopBackpackWrapper.INSTANCE;
	}

	public BackpackContext getBackpackContext() {
		return backpackContext;
	}

	public boolean canDisableUpgrade(int upgradeSlot) {
		Map<Integer, IUpgradeWrapper> slotWrappers = backpackWrapper.getUpgradeHandler().getSlotWrappers();
		if (!slotWrappers.containsKey(upgradeSlot)) {
			return false;
		}
		return slotWrappers.get(upgradeSlot).canBeDisabled();
	}

	public boolean getUpgradeEnabled(int upgradeSlot) {
		Map<Integer, IUpgradeWrapper> slotWrappers = backpackWrapper.getUpgradeHandler().getSlotWrappers();
		if (!slotWrappers.containsKey(upgradeSlot)) {
			return false;
		}
		return slotWrappers.get(upgradeSlot).isEnabled();
	}

	public void setUpgradeEnabled(int upgradeSlot, boolean enabled) {
		Map<Integer, IUpgradeWrapper> slotWrappers = backpackWrapper.getUpgradeHandler().getSlotWrappers();
		if (!slotWrappers.containsKey(upgradeSlot)) {
			return;
		}
		if (isClientSide()) {
			CompoundNBT data = new CompoundNBT();
			data.putBoolean(UPGRADE_ENABLED_TAG, enabled);
			data.putInt(UPGRADE_SLOT_TAG, upgradeSlot);
			PacketHandler.sendToServer(new ServerBackpackDataMessage(data));
		}
		slotWrappers.get(upgradeSlot).setEnabled(enabled);
	}

	public Optional<UpgradeContainerBase<?, ?>> getOpenContainer() {
		return backpackWrapper.getOpenTabId().flatMap(id -> upgradeContainers.containsKey(id) ? Optional.of(upgradeContainers.get(id)) : Optional.empty());
	}

	public class BackpackUpgradeSlot extends SlotItemHandler {
		private boolean wasEmpty = false;

		public BackpackUpgradeSlot(BackpackUpgradeHandler upgradeHandler, int slotIndex, int yPosition) {
			super(upgradeHandler, slotIndex, -15, yPosition);
		}

		@Override
		public void onSlotChanged() {
			super.onSlotChanged();
			if ((!isUpdatingFromPacket && wasEmpty != getStack().isEmpty()) || updateWrappersAndCheckForReloadNeeded()) {
				reloadUpgradeControl();
				if (!isFirstLevelBackpack()) {
					parentBackpackWrapper.getUpgradeHandler().refreshUpgradeWrappers();
				}
			}
			wasEmpty = getStack().isEmpty();
		}

		@Override
		public boolean isItemValid(ItemStack stack) {
			if (stack.isEmpty()) {
				return false;
			}
			return stack.getItem() instanceof IBackpackUpgradeItem && ((IBackpackUpgradeItem<?>) stack.getItem()).canAddUpgradeTo(backpackWrapper, isFirstLevelBackpack());
		}

		@Override
		public boolean canTakeStack(PlayerEntity playerIn) {
			return super.canTakeStack(playerIn) && ((IBackpackUpgradeItem<?>) getStack().getItem()).canRemoveUpgradeFrom(backpackWrapper);
		}

		public boolean canSwapStack(PlayerEntity player, ItemStack stackToPut) {
			return super.canTakeStack(player) && ((IBackpackUpgradeItem<?>) getStack().getItem()).canSwapUpgradeFor(stackToPut, backpackWrapper);
		}

		private boolean updateWrappersAndCheckForReloadNeeded() {
			int checkedContainersCount = 0;
			for (Map.Entry<Integer, IUpgradeWrapper> slotWrapper : backpackWrapper.getUpgradeHandler().getSlotWrappers().entrySet()) {
				UpgradeContainerBase<?, ?> container = upgradeContainers.get(slotWrapper.getKey());
				if (slotWrapper.getValue().hideSettingsTab()) {
					if (container != null) {
						return true;
					}
				} else if (container == null || container.getUpgradeWrapper().isEnabled() != slotWrapper.getValue().isEnabled()) {
					return true;
				} else if (container.getUpgradeWrapper() != slotWrapper.getValue()) {
					if (container.getUpgradeWrapper().getUpgradeStack().getItem() != slotWrapper.getValue().getUpgradeStack().getItem()) {
						return true;
					} else {
						container.setUpgradeWrapper(slotWrapper.getValue());
						checkedContainersCount++;
					}
				}
			}
			return checkedContainersCount != upgradeContainers.size();
		}

		private void reloadUpgradeControl() {
			backpackWrapper.removeOpenTabId();
			removeUpgradeSettingsSlots();
			upgradeContainers.clear();
			addUpgradeSettingsContainers(player);
			onUpgradesChanged();
		}

		private void removeUpgradeSettingsSlots() {
			List<Integer> slotNumbersToRemove = new ArrayList<>();
			for (UpgradeContainerBase<?, ?> container : upgradeContainers.values()) {
				container.getSlots().forEach(slot -> {
					int upgradeSlotIndex = slot.slotNumber - inventorySlots.size();
					slotNumbersToRemove.add(upgradeSlotIndex);
					upgradeSlots.remove(slot);
				});
			}
			slotNumbersToRemove.sort(IntComparators.OPPOSITE_COMPARATOR);
			for (int slotNumber : slotNumbersToRemove) {
				upgradeItemStacks.remove(slotNumber);
			}
		}

		private void onUpgradesChanged() {
			if (upgradeChangeListener != null) {
				upgradeChangeListener.accept(BackpackContainer.this);
			}
		}

		@Nullable
		@Override
		public Pair<ResourceLocation, ResourceLocation> getBackground() {
			return new Pair<>(PlayerContainer.LOCATION_BLOCKS_TEXTURE, EMPTY_UPGRADE_SLOT_BACKGROUND);
		}
	}

	@SuppressWarnings("unchecked") // both conditions of T are checked before casting it in the result
	public <T extends UpgradeContainerBase<?, ?> & ICraftingContainer> Optional<T> getOpenOrFirstCraftingContainer() {
		T firstContainer = null;
		for (UpgradeContainerBase<?, ?> container : upgradeContainers.values()) {
			if (container instanceof ICraftingContainer) {
				if (container.isOpen()) {
					return Optional.of((T) container);
				} else if (firstContainer == null) {
					firstContainer = (T) container;
				}
			}
		}
		return Optional.ofNullable(firstContainer);
	}

	@Override
	public NonNullList<ItemStack> getInventory() {
		NonNullList<ItemStack> list = super.getInventory();
		upgradeSlots.forEach(upgradeSlot -> list.add(upgradeSlot.getStack()));
		return list;
	}

	@Override
	public void detectAndSendChanges() {
		if (backpackSlotNumber != -1) {
			closeBackpackScreenIfSomethingMessedWithBackpackStack(getSlot(backpackSlotNumber).getStack());
		}
		super.detectAndSendChanges();
		for (int i = 0; i < upgradeSlots.size(); ++i) {
			Slot slot = upgradeSlots.get(i);
			ItemStack currentStack = slot.getStack();
			ItemStack previousStack = upgradeItemStacks.get(i);
			if (!ItemStack.areItemStacksEqual(previousStack, currentStack)) {
				boolean clientStackChanged = !previousStack.equals(currentStack, true);
				ItemStack stackCopy = currentStack.copy();
				upgradeItemStacks.set(i, stackCopy);

				if (clientStackChanged) {
					for (IContainerListener icontainerlistener : listeners) {
						icontainerlistener.sendSlotContents(this, slot.slotNumber, stackCopy);
					}
				}
			}
		}
	}

	@Override
	public Slot getSlot(int slotId) {
		return slotId < inventorySlots.size() ? super.getSlot(slotId) : upgradeSlots.get(slotId - inventorySlots.size());
	}

	private static final Method ON_SWAP_CRAFT = ObfuscationReflectionHelper.findMethod(Slot.class, "func_190900_b", int.class);

	private void onSwapCraft(Slot slot, int numItemsCrafted) {
		try {
			ON_SWAP_CRAFT.invoke(slot, numItemsCrafted);
		}
		catch (IllegalAccessException | InvocationTargetException e) {
			SophisticatedBackpacks.LOGGER.error("Error invoking onSwapCraft method in Slot class", e);
		}
	}

	//copy of Container's func_241440_b_ with the replacement of inventorySlots.get to getSlot, call to onswapcraft as that's protected in vanilla and an addition of upgradeSlots to pickup all
	@SuppressWarnings("java:S3776")
	//complexity here is brutal, but it's something that's in vanilla and need to keep this as close to it as possible for easier ports
	@Override
	protected ItemStack func_241440_b_(int slotId, int dragType, ClickType clickType, PlayerEntity player) {
		ItemStack ret = ItemStack.EMPTY;
		PlayerInventory playerinventory = player.inventory;
		if (clickType == ClickType.QUICK_CRAFT) {
			int prevDragEvent = dragEvent;
			dragEvent = getDragEvent(dragType);
			if ((prevDragEvent != 1 || dragEvent != 2) && prevDragEvent != dragEvent) {
				resetDrag();
			} else if (playerinventory.getItemStack().isEmpty()) {
				resetDrag();
			} else if (dragEvent == 0) {
				dragMode = extractDragMode(dragType);
				if (isValidDragMode(dragMode, player)) {
					dragEvent = 1;
					dragSlots.clear();
				} else {
					resetDrag();
				}
			} else if (dragEvent == 1) {
				Slot slot7 = getSlot(slotId);
				ItemStack itemstack12 = playerinventory.getItemStack();
				if (canMergeItemToSlot(slot7, itemstack12) && slot7.isItemValid(itemstack12) && (dragMode == 2 || itemstack12.getCount() > dragSlots.size()) && canDragIntoSlot(slot7)) {
					dragSlots.add(slot7);
				}
			} else if (dragEvent == 2) {
				if (!dragSlots.isEmpty()) {
					ItemStack cursorStack = playerinventory.getItemStack().copy();
					int k1 = playerinventory.getItemStack().getCount();

					for (Slot slot8 : dragSlots) {
						ItemStack itemstack13 = playerinventory.getItemStack();
						if (slot8 != null && canMergeItemToSlot(slot8, itemstack13) && slot8.isItemValid(itemstack13) && (dragMode == 2 || itemstack13.getCount() >= dragSlots.size()) && canDragIntoSlot(slot8)) {
							ItemStack itemstack14 = cursorStack.copy();
							int j3 = slot8.getHasStack() ? slot8.getStack().getCount() : 0;
							computeStackSize(dragSlots, dragMode, itemstack14, j3);
							int slotStackLimit = slot8.getItemStackLimit(itemstack14);
							if (!(slot8 instanceof BackpackInventorySlot) && slotStackLimit > cursorStack.getMaxStackSize()) {
								slotStackLimit = cursorStack.getMaxStackSize();
							}
							if (itemstack14.getCount() > slotStackLimit) {
								itemstack14.setCount(slotStackLimit);
							}
							k1 -= itemstack14.getCount() - j3;
							slot8.putStack(itemstack14);
						}
					}

					cursorStack.setCount(k1);
					playerinventory.setItemStack(cursorStack);
				}

				resetDrag();
			} else {
				resetDrag();
			}
		} else if (dragEvent != 0) {
			resetDrag();
		} else if ((clickType == ClickType.PICKUP || clickType == ClickType.QUICK_MOVE) && (dragType == 0 || dragType == 1)) {
			if (slotId == -999) {
				if (!playerinventory.getItemStack().isEmpty()) {
					if (dragType == 0) {
						player.dropItem(playerinventory.getItemStack(), true);
						playerinventory.setItemStack(ItemStack.EMPTY);
					}

					if (dragType == 1) {
						player.dropItem(playerinventory.getItemStack().split(1), true);
					}
				}
			} else if (clickType == ClickType.QUICK_MOVE) {
				if (slotId < 0) {
					return ItemStack.EMPTY;
				}

				Slot slot5 = getSlot(slotId);
				if (!slot5.canTakeStack(player)) {
					return ItemStack.EMPTY;
				}

				if (isBackpackInventorySlot(slotId)) {
					ret = transferStackInSlot(player, slotId).copy();
				} else {
					for (ItemStack itemstack8 = transferStackInSlot(player, slotId);
						 !itemstack8.isEmpty() && ItemStack.areItemsEqual(slot5.getStack(), itemstack8);
						 itemstack8 = transferStackInSlot(player, slotId)) {
						ret = itemstack8.copy();
					}
				}
			} else {
				if (slotId < 0) {
					return ItemStack.EMPTY;
				}

				Slot slot6 = getSlot(slotId);
				ItemStack slotStack = slot6.getStack();
				ItemStack cursorStack = playerinventory.getItemStack();
				if (!slotStack.isEmpty()) {
					ret = slotStack.copy();
				}

				if (slotStack.isEmpty()) {
					if (!cursorStack.isEmpty() && slot6.isItemValid(cursorStack)) {
						int j2 = dragType == 0 ? cursorStack.getCount() : 1;
						if (j2 > slot6.getItemStackLimit(cursorStack)) {
							j2 = slot6.getItemStackLimit(cursorStack);
						}

						slot6.putStack(cursorStack.split(j2));
					}
				} else if (slot6.canTakeStack(player)) {
					if (cursorStack.isEmpty()) {
						if (slotStack.isEmpty()) {
							slot6.putStack(ItemStack.EMPTY);
							playerinventory.setItemStack(ItemStack.EMPTY);
						} else {
							int k2 = dragType == 0 ? Math.min(slotStack.getCount(), slotStack.getMaxStackSize()) : Math.min(slotStack.getMaxStackSize() + 1, slotStack.getCount() + 1) / 2;
							playerinventory.setItemStack(slot6.decrStackSize(k2));
							if (slotStack.isEmpty()) {
								slot6.putStack(ItemStack.EMPTY);
							}

							slot6.onTake(player, playerinventory.getItemStack());
						}
					} else if (slot6.isItemValid(cursorStack)) {
						if (areItemsAndTagsEqual(slotStack, cursorStack)) {
							int countToInsert = dragType == 0 ? cursorStack.getCount() : 1;
							if (countToInsert > slot6.getItemStackLimit(cursorStack) - slotStack.getCount()) {
								countToInsert = slot6.getItemStackLimit(cursorStack) - slotStack.getCount();
							}

							if (!(slot6 instanceof BackpackInventorySlot) && countToInsert > cursorStack.getMaxStackSize() - slotStack.getCount()) {
								countToInsert = cursorStack.getMaxStackSize() - slotStack.getCount();
							}

							cursorStack.shrink(countToInsert);
							slotStack.grow(countToInsert);
						} else if (cursorStack.getCount() <= slot6.getItemStackLimit(cursorStack) && slotStack.getCount() <= slotStack.getMaxStackSize()) {
							slot6.putStack(cursorStack);
							playerinventory.setItemStack(slotStack);
						}
					} else if (cursorStack.getMaxStackSize() > 1 && areItemsAndTagsEqual(slotStack, cursorStack) && !slotStack.isEmpty()) {
						int i3 = slotStack.getCount();
						if (i3 + cursorStack.getCount() <= cursorStack.getMaxStackSize()) {
							cursorStack.grow(i3);
							slotStack = slot6.decrStackSize(i3);
							if (slotStack.isEmpty()) {
								slot6.putStack(ItemStack.EMPTY);
							}

							slot6.onTake(player, playerinventory.getItemStack());
						}
					}
				}

				slot6.onSlotChanged();
			}
		} else if (clickType == ClickType.SWAP) {
			Slot slot = getSlot(slotId);
			ItemStack cursorStack = playerinventory.getStackInSlot(dragType);
			ItemStack slotStack = slot.getStack();
			if (!cursorStack.isEmpty() || !slotStack.isEmpty()) {
				if (cursorStack.isEmpty()) {
					if (slot.canTakeStack(player)) {
						if (slotStack.getCount() <= slotStack.getMaxStackSize()) {
							playerinventory.setInventorySlotContents(dragType, slotStack);
							onSwapCraft(slot, slotStack.getCount());
							slot.putStack(ItemStack.EMPTY);
							slot.onTake(player, slotStack);
						} else {
							playerinventory.setInventorySlotContents(dragType, slotStack.split(slotStack.getMaxStackSize()));
							slot.onSlotChanged();
						}
					}
				} else if (slotStack.isEmpty()) {
					if (slot.isItemValid(cursorStack)) {
						int i = slot.getItemStackLimit(cursorStack);
						if (cursorStack.getCount() > i) {
							slot.putStack(cursorStack.split(i));
						} else {
							slot.putStack(cursorStack);
							playerinventory.setInventorySlotContents(dragType, ItemStack.EMPTY);
						}
					}
				} else if (slotStack.getCount() <= slotStack.getMaxStackSize() && slot.canTakeStack(player) && slot.isItemValid(cursorStack)) {
					int l1 = slot.getItemStackLimit(cursorStack);
					if (cursorStack.getCount() > l1) {
						slot.putStack(cursorStack.split(l1));
						slot.onTake(player, slotStack);
						if (!playerinventory.addItemStackToInventory(slotStack)) {
							player.dropItem(slotStack, true);
						}
					} else {
						slot.putStack(cursorStack);
						playerinventory.setInventorySlotContents(dragType, slotStack);
						slot.onTake(player, slotStack);
					}
				}
			}
		} else if (clickType == ClickType.CLONE && player.abilities.isCreativeMode && playerinventory.getItemStack().isEmpty() && slotId >= 0) {
			Slot slot4 = getSlot(slotId);
			if (slot4.getHasStack()) {
				ItemStack itemstack7 = slot4.getStack().copy();
				itemstack7.setCount(itemstack7.getMaxStackSize());
				playerinventory.setItemStack(itemstack7);
			}
		} else if (clickType == ClickType.THROW && playerinventory.getItemStack().isEmpty() && slotId >= 0) {
			Slot slot3 = getSlot(slotId);
			if (slot3.getHasStack() && slot3.canTakeStack(player)) {
				ItemStack stackToThrow = slot3.decrStackSize(dragType == 0 ? 1 : Math.min(slot3.getStack().getCount(), slot3.getStack().getMaxStackSize()));
				slot3.onTake(player, stackToThrow);
				player.dropItem(stackToThrow, true);
			}
		} else if (clickType == ClickType.PICKUP_ALL && slotId >= 0) {
			Slot slot2 = getSlot(slotId);
			ItemStack cursorStack = playerinventory.getItemStack();
			if (!cursorStack.isEmpty() && (!slot2.getHasStack() || !slot2.canTakeStack(player))) {
				int j1 = dragType == 0 ? 0 : inventorySlots.size() - 1;
				int i2 = dragType == 0 ? 1 : -1;

				for (int j = 0; j < 2; ++j) {
					for (int k = j1; k >= 0 && k < inventorySlots.size() && cursorStack.getCount() < cursorStack.getMaxStackSize(); k += i2) {
						Slot slot1 = inventorySlots.get(k);
						if (slot1.getHasStack() && canMergeItemToSlot(slot1, cursorStack) && slot1.canTakeStack(player) && canMergeSlot(cursorStack, slot1)) {
							ItemStack itemstack3 = slot1.getStack();
							if (j != 0 || itemstack3.getCount() != itemstack3.getMaxStackSize()) {
								int l = Math.min(cursorStack.getMaxStackSize() - cursorStack.getCount(), itemstack3.getCount());
								ItemStack itemstack4 = slot1.decrStackSize(l);
								cursorStack.grow(l);
								if (itemstack4.isEmpty()) {
									slot1.putStack(ItemStack.EMPTY);
								}

								slot1.onTake(player, itemstack4);
							}
						}
					}
				}

				j1 = dragType == 0 ? 0 : upgradeSlots.size() - 1;

				for (int j = 0; j < 2; ++j) {
					for (int upgradeSlotId = j1; upgradeSlotId >= 0 && upgradeSlotId < upgradeSlots.size() && cursorStack.getCount() < cursorStack.getMaxStackSize(); upgradeSlotId += i2) {
						Slot upgradeSlot = upgradeSlots.get(upgradeSlotId);
						if (upgradeSlot.getHasStack() && canMergeItemToSlot(upgradeSlot, cursorStack) && upgradeSlot.canTakeStack(player) && canMergeSlot(cursorStack, upgradeSlot)) {
							ItemStack itemstack3 = upgradeSlot.getStack();
							if (j != 0 || itemstack3.getCount() != itemstack3.getMaxStackSize()) {
								int l = Math.min(cursorStack.getMaxStackSize() - cursorStack.getCount(), itemstack3.getCount());
								ItemStack itemstack4 = upgradeSlot.decrStackSize(l);
								cursorStack.grow(l);
								if (itemstack4.isEmpty()) {
									upgradeSlot.putStack(ItemStack.EMPTY);
								}

								upgradeSlot.onTake(player, itemstack4);
							}
						}
					}
				}
			}

			detectAndSendChanges();
		}

		sendSlotUpdates();

		return ret;
	}

	public void sendSlotUpdates() {
		if (!player.world.isRemote) {
			ServerPlayerEntity serverPlayer = (ServerPlayerEntity) player;
			slotStacksToUpdate.forEach((slot, stack) -> serverPlayer.connection.sendPacket(new SSetSlotPacket(serverPlayer.openContainer.windowId, slot, stack)));
			slotStacksToUpdate.clear();
		}
	}

	public static boolean canMergeItemToSlot(@Nullable Slot slot, ItemStack stack) {
		boolean flag = slot == null || !slot.getHasStack();
		if (!flag && stack.isItemEqual(slot.getStack()) && ItemStack.areItemStackTagsEqual(slot.getStack(), stack)) {
			return slot.getStack().getCount() <= calculateMaxCountForStack(slot.getSlotStackLimit(), stack);
		} else {
			return flag;
		}
	}

	private static int calculateMaxCountForStack(int slotLimit, ItemStack stack) {
		return slotLimit / 64 * stack.getMaxStackSize();
	}

	@Override
	protected boolean mergeItemStack(ItemStack stack, int startIndex, int endIndex, boolean reverseDirection) {
		return mergeItemStack(stack, startIndex, endIndex, reverseDirection, false);
	}

	//copy of mergeItemStack from Container - just calling getSlot here to account for upgrade slots instead of direct inventorySlots.get
	// and minor addition to be able to ignore max stack size
	@SuppressWarnings({"java:S3776", "java:S135"})
	//need to keep this very close to vanilla for easy port so not refactoring it to lower complexity or less exit points in loops
	protected boolean mergeItemStack(ItemStack sourceStack, int startIndex, int endIndex, boolean reverseDirection, boolean transferMaxStackSizeFromSource) {
		boolean flag = false;
		int i = startIndex;
		if (reverseDirection) {
			i = endIndex - 1;
		}

		int toTransfer = transferMaxStackSizeFromSource ? Math.min(sourceStack.getMaxStackSize(), sourceStack.getCount()) : sourceStack.getCount();
		if (sourceStack.isStackable() || getSlot(startIndex).getSlotStackLimit() > 64) {
			while (toTransfer > 0) {
				if (reverseDirection) {
					if (i < startIndex) {
						break;
					}
				} else if (i >= endIndex) {
					break;
				}

				Slot slot = getSlot(i);
				ItemStack destStack = slot.getStack();
				if (!destStack.isEmpty() && areItemsAndTagsEqual(sourceStack, destStack)) {
					int j = destStack.getCount() + toTransfer;
					int maxSize = calculateMaxCountForStack(slot.getSlotStackLimit(), sourceStack);
					if (j <= maxSize) {
						sourceStack.shrink(toTransfer);
						destStack.setCount(j);
						toTransfer = 0;
						slot.onSlotChanged();
						flag = true;
					} else if (destStack.getCount() < maxSize) {
						sourceStack.shrink(maxSize - destStack.getCount());
						toTransfer -= maxSize - destStack.getCount();
						destStack.setCount(maxSize);
						slot.onSlotChanged();
						flag = true;
					}
				}

				if (reverseDirection) {
					--i;
				} else {
					++i;
				}
			}
		}

		if (toTransfer > 0) {
			if (reverseDirection) {
				i = endIndex - 1;
			} else {
				i = startIndex;
			}

			while (true) {
				if (reverseDirection) {
					if (i < startIndex) {
						break;
					}
				} else if (i >= endIndex) {
					break;
				}

				Slot destStack = getSlot(i);
				ItemStack itemstack1 = destStack.getStack();
				if (itemstack1.isEmpty() && destStack.isItemValid(sourceStack)) {
					//filter slot logic
					if (destStack instanceof IFilterSlot) {
						ItemStack fakeStack = sourceStack.copy();
						fakeStack.setCount(1);
						destStack.putStack(fakeStack);
						//end filter slot logic
					} else if (toTransfer > destStack.getSlotStackLimit()) {
						destStack.putStack(sourceStack.split(destStack.getSlotStackLimit()));
					} else {
						destStack.putStack(sourceStack.split(toTransfer));
					}

					destStack.onSlotChanged();
					flag = true;
					break;
				}

				if (reverseDirection) {
					--i;
				} else {
					++i;
				}
			}
		}

		return flag;
	}

	@Override
	public void addListener(IContainerListener listener) {
		if (listener instanceof ServerPlayerEntity && backpackWrapper.getInventoryHandler().getStackSizeMultiplier() > 1) {
			super.addListener(new HighStackCountListener((ServerPlayerEntity) listener));
			return;
		}
		super.addListener(listener);
	}

	@Override
	public void onContainerClosed(PlayerEntity player) {
		for (Slot slot : upgradeSlots) {
			if (isInventorySlotInUpgradeTab(player, slot) && slot.getStack().getItem() instanceof BackpackItem &&
					!backpackWrapper.getInventoryHandler().isItemValid(0, slot.getStack())) {
				ItemStack slotStack = slot.getStack();
				slot.putStack(ItemStack.EMPTY);
				if (!player.addItemStackToInventory(slotStack)) {
					player.dropItem(slotStack, false);
				}
			}
		}
		super.onContainerClosed(player);
	}

	private boolean isInventorySlotInUpgradeTab(PlayerEntity player, Slot slot) {
		return slot.canTakeStack(player) && !(slot instanceof CraftingResultSlot);
	}

	public void setSlotStackToUpdate(int slot, ItemStack stack) {
		slotStacksToUpdate.put(slot, stack);
	}
}
