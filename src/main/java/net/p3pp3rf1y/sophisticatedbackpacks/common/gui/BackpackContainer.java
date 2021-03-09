package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.Lists;
import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.ints.IntComparators;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.NonNullList;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackUpgradeHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.NoopBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackBackgroundProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.ServerBackpackDataMessage;

import javax.annotation.Nullable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
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

	public BackpackContainer(int windowId, PlayerEntity player, BackpackContext backpackContext) {
		super(backpackContext.getContainerType(), windowId);
		this.player = player;
		this.backpackContext = backpackContext;
		parentBackpackWrapper = backpackContext.getParentBackpackWrapper(player).orElse(NoopBackpackWrapper.INSTANCE);
		backpackWrapper = backpackContext.getBackpackWrapper(player);
		backpackWrapper.fillWithLoot(player);
		backpackBackgroundProperties = getNumberOfSlots() <= 81 ? BackpackBackgroundProperties.REGULAR : BackpackBackgroundProperties.WIDE;

		initSlotsAndContainers(player, backpackContext.getBackpackSlotIndex(), backpackContext.shouldLockBackpackSlot());
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
		IItemHandlerModifiable inventoryHandler = backpackWrapper.getInventoryHandler();
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
		super.setAll(items);
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
	public boolean canInteractWith(PlayerEntity playerIn) {
		return true;
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

			if (!mergeSlotStack(slot, index, slotStack)) {
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

	private boolean mergeSlotStack(Slot slot, int index, ItemStack slotStack) {
		if (isBackpackInventoryOrUpgradeSlot(index)) {
			return mergeStackToOpenUpgradeTab(slotStack) || mergeStackToPlayersInventory(slotStack);
		} else if (isUpgradeSettingsSlot(index)) {
			if (getSlotUpgradeContainer(slot).map(c -> c.mergeIntoBackpackFirst(slot)).orElse(true)) {
				return mergeStackToBackpack(slotStack) || mergeStackToPlayersInventory(slotStack);
			}
			return mergeStackToPlayersInventory(slotStack) || mergeStackToBackpack(slotStack);
		} else {
			return mergeStackToOpenUpgradeTab(slotStack) || mergeStackToUpgradeSlots(slotStack) || mergeStackToBackpack(slotStack);
		}
	}

	private boolean mergeStackToUpgradeSlots(ItemStack slotStack) {
		return mergeItemStack(slotStack, inventorySlots.size(), inventorySlots.size() + getNumberOfUpgradeSlots(), false);
	}

	private boolean mergeStackToOpenUpgradeTab(ItemStack slotStack) {
		return getOpenContainer().map(c -> {
			List<Slot> slots = c.getSlots();
			if (slots.isEmpty()) {
				return false;
			}
			int firstSlotIndex = slots.get(0).slotNumber;
			int lastSlotIndex = slots.get(slots.size() - 1).slotNumber;
			return mergeItemStack(slotStack, firstSlotIndex, lastSlotIndex + 1, false);
		}).orElse(false);
	}

	private boolean mergeStackToBackpack(ItemStack slotStack) {
		return mergeItemStack(slotStack, 0, getNumberOfSlots(), false);
	}

	private boolean mergeStackToPlayersInventory(ItemStack slotStack) {
		return mergeItemStack(slotStack, getNumberOfSlots(), inventorySlots.size(), true);
	}

	private boolean isUpgradeSettingsSlot(int index) {
		return index >= getNumberOfSlots() + getNumberOfUpgradeSlots() + NUMBER_OF_PLAYER_SLOTS;
	}

	private boolean isBackpackInventoryOrUpgradeSlot(int index) {
		return index < getNumberOfSlots() || (index >= inventorySlots.size() && (index - inventorySlots.size() < getNumberOfUpgradeSlots()));
	}

	public Optional<UpgradeContainerBase<?, ?>> getSlotUpgradeContainer(Slot slot) {
		if (slot.slotNumber >= getFirstUpgradeSettingsSlot()) {
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
		if (slotId >= getFirstUpgradeSettingsSlot() && getSlot(slotId) instanceof FilterSlotItemHandler && getSlot(slotId).isItemValid(player.inventory.getItemStack())) {
			ItemStack currentStack = player.inventory.getItemStack().copy();
			if (currentStack.getCount() > 1) {
				currentStack.setCount(1);
			}

			getSlot(slotId).putStack(currentStack);
			return ItemStack.EMPTY;
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

		public BackpackUpgradeSlot(BackpackUpgradeHandler upgradeHandler, int slotIndex, int yPosition) {
			super(upgradeHandler, slotIndex, -15, yPosition);
		}

		@Override
		public void onSlotChanged() {
			super.onSlotChanged();
			if (updateWrappersAndCheckForReloadNeeded()) {
				reloadUpgradeControl();
				if (!isFirstLevelBackpack()) {
					parentBackpackWrapper.getUpgradeHandler().refreshUpgradeWrappers();
				}
			}
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

	public Optional<ICraftingContainer> getCraftingContainer() {
		for (UpgradeContainerBase<?, ?> container : upgradeContainers.values()) {
			if (container instanceof ICraftingContainer) {
				return Optional.of((ICraftingContainer) container);
			}
		}
		return Optional.empty();
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
				if (canAddItemToSlot(slot7, itemstack12, true) && slot7.isItemValid(itemstack12) && (dragMode == 2 || itemstack12.getCount() > dragSlots.size()) && canDragIntoSlot(slot7)) {
					dragSlots.add(slot7);
				}
			} else if (dragEvent == 2) {
				if (!dragSlots.isEmpty()) {
					ItemStack itemstack10 = playerinventory.getItemStack().copy();
					int k1 = playerinventory.getItemStack().getCount();

					for (Slot slot8 : dragSlots) {
						ItemStack itemstack13 = playerinventory.getItemStack();
						if (slot8 != null && canAddItemToSlot(slot8, itemstack13, true) && slot8.isItemValid(itemstack13) && (dragMode == 2 || itemstack13.getCount() >= dragSlots.size()) && canDragIntoSlot(slot8)) {
							ItemStack itemstack14 = itemstack10.copy();
							int j3 = slot8.getHasStack() ? slot8.getStack().getCount() : 0;
							computeStackSize(dragSlots, dragMode, itemstack14, j3);
							int k3 = Math.min(itemstack14.getMaxStackSize(), slot8.getItemStackLimit(itemstack14));
							if (itemstack14.getCount() > k3) {
								itemstack14.setCount(k3);
							}

							k1 -= itemstack14.getCount() - j3;
							slot8.putStack(itemstack14);
						}
					}

					itemstack10.setCount(k1);
					playerinventory.setItemStack(itemstack10);
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

				for (ItemStack itemstack8 = transferStackInSlot(player, slotId); !itemstack8.isEmpty() && ItemStack.areItemsEqual(slot5.getStack(), itemstack8); itemstack8 = transferStackInSlot(player, slotId)) {
					ret = itemstack8.copy();
				}
			} else {
				if (slotId < 0) {
					return ItemStack.EMPTY;
				}

				Slot slot6 = getSlot(slotId);
				ItemStack itemstack9 = slot6.getStack();
				ItemStack itemstack11 = playerinventory.getItemStack();
				if (!itemstack9.isEmpty()) {
					ret = itemstack9.copy();
				}

				if (itemstack9.isEmpty()) {
					if (!itemstack11.isEmpty() && slot6.isItemValid(itemstack11)) {
						int j2 = dragType == 0 ? itemstack11.getCount() : 1;
						if (j2 > slot6.getItemStackLimit(itemstack11)) {
							j2 = slot6.getItemStackLimit(itemstack11);
						}

						slot6.putStack(itemstack11.split(j2));
					}
				} else if (slot6.canTakeStack(player)) {
					if (itemstack11.isEmpty()) {
						if (itemstack9.isEmpty()) {
							slot6.putStack(ItemStack.EMPTY);
							playerinventory.setItemStack(ItemStack.EMPTY);
						} else {
							int k2 = dragType == 0 ? itemstack9.getCount() : (itemstack9.getCount() + 1) / 2;
							playerinventory.setItemStack(slot6.decrStackSize(k2));
							if (itemstack9.isEmpty()) {
								slot6.putStack(ItemStack.EMPTY);
							}

							slot6.onTake(player, playerinventory.getItemStack());
						}
					} else if (slot6.isItemValid(itemstack11)) {
						if (areItemsAndTagsEqual(itemstack9, itemstack11)) {
							int l2 = dragType == 0 ? itemstack11.getCount() : 1;
							if (l2 > slot6.getItemStackLimit(itemstack11) - itemstack9.getCount()) {
								l2 = slot6.getItemStackLimit(itemstack11) - itemstack9.getCount();
							}

							if (l2 > itemstack11.getMaxStackSize() - itemstack9.getCount()) {
								l2 = itemstack11.getMaxStackSize() - itemstack9.getCount();
							}

							itemstack11.shrink(l2);
							itemstack9.grow(l2);
						} else if (itemstack11.getCount() <= slot6.getItemStackLimit(itemstack11)) {
							slot6.putStack(itemstack11);
							playerinventory.setItemStack(itemstack9);
						}
					} else if (itemstack11.getMaxStackSize() > 1 && areItemsAndTagsEqual(itemstack9, itemstack11) && !itemstack9.isEmpty()) {
						int i3 = itemstack9.getCount();
						if (i3 + itemstack11.getCount() <= itemstack11.getMaxStackSize()) {
							itemstack11.grow(i3);
							itemstack9 = slot6.decrStackSize(i3);
							if (itemstack9.isEmpty()) {
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
			ItemStack itemstack1 = playerinventory.getStackInSlot(dragType);
			ItemStack itemstack2 = slot.getStack();
			if (!itemstack1.isEmpty() || !itemstack2.isEmpty()) {
				if (itemstack1.isEmpty()) {
					if (slot.canTakeStack(player)) {
						playerinventory.setInventorySlotContents(dragType, itemstack2);
						onSwapCraft(slot, itemstack2.getCount());
						slot.putStack(ItemStack.EMPTY);
						slot.onTake(player, itemstack2);
					}
				} else if (itemstack2.isEmpty()) {
					if (slot.isItemValid(itemstack1)) {
						int i = slot.getItemStackLimit(itemstack1);
						if (itemstack1.getCount() > i) {
							slot.putStack(itemstack1.split(i));
						} else {
							slot.putStack(itemstack1);
							playerinventory.setInventorySlotContents(dragType, ItemStack.EMPTY);
						}
					}
				} else if (slot.canTakeStack(player) && slot.isItemValid(itemstack1)) {
					int l1 = slot.getItemStackLimit(itemstack1);
					if (itemstack1.getCount() > l1) {
						slot.putStack(itemstack1.split(l1));
						slot.onTake(player, itemstack2);
						if (!playerinventory.addItemStackToInventory(itemstack2)) {
							player.dropItem(itemstack2, true);
						}
					} else {
						slot.putStack(itemstack1);
						playerinventory.setInventorySlotContents(dragType, itemstack2);
						slot.onTake(player, itemstack2);
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
				ItemStack itemstack6 = slot3.decrStackSize(dragType == 0 ? 1 : slot3.getStack().getCount());
				slot3.onTake(player, itemstack6);
				player.dropItem(itemstack6, true);
			}
		} else if (clickType == ClickType.PICKUP_ALL && slotId >= 0) {
			Slot slot2 = getSlot(slotId);
			ItemStack itemstack5 = playerinventory.getItemStack();
			if (!itemstack5.isEmpty() && (!slot2.getHasStack() || !slot2.canTakeStack(player))) {
				int j1 = dragType == 0 ? 0 : inventorySlots.size() - 1;
				int i2 = dragType == 0 ? 1 : -1;

				for (int j = 0; j < 2; ++j) {
					for (int k = j1; k >= 0 && k < inventorySlots.size() && itemstack5.getCount() < itemstack5.getMaxStackSize(); k += i2) {
						Slot slot1 = inventorySlots.get(k);
						if (slot1.getHasStack() && canAddItemToSlot(slot1, itemstack5, true) && slot1.canTakeStack(player) && canMergeSlot(itemstack5, slot1)) {
							ItemStack itemstack3 = slot1.getStack();
							if (j != 0 || itemstack3.getCount() != itemstack3.getMaxStackSize()) {
								int l = Math.min(itemstack5.getMaxStackSize() - itemstack5.getCount(), itemstack3.getCount());
								ItemStack itemstack4 = slot1.decrStackSize(l);
								itemstack5.grow(l);
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
					for (int upgradeSlotId = j1; upgradeSlotId >= 0 && upgradeSlotId < upgradeSlots.size() && itemstack5.getCount() < itemstack5.getMaxStackSize(); upgradeSlotId += i2) {
						Slot upgradeSlot = upgradeSlots.get(upgradeSlotId);
						if (upgradeSlot.getHasStack() && canAddItemToSlot(upgradeSlot, itemstack5, true) && upgradeSlot.canTakeStack(player) && canMergeSlot(itemstack5, upgradeSlot)) {
							ItemStack itemstack3 = upgradeSlot.getStack();
							if (j != 0 || itemstack3.getCount() != itemstack3.getMaxStackSize()) {
								int l = Math.min(itemstack5.getMaxStackSize() - itemstack5.getCount(), itemstack3.getCount());
								ItemStack itemstack4 = upgradeSlot.decrStackSize(l);
								itemstack5.grow(l);
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

		return ret;
	}

	//copy of mergeItemStack from Container - just calling getSlot here to account for upgrade slots instead of direct inventorySlots.get
	@SuppressWarnings({"java:S3776", "java:S135"})
	//need to keep this very close to vanilla for easy port so not refactoring it to lower complexity or less exit points in loops
	@Override
	protected boolean mergeItemStack(ItemStack stack, int startIndex, int endIndex, boolean reverseDirection) {
		boolean flag = false;
		int i = startIndex;
		if (reverseDirection) {
			i = endIndex - 1;
		}

		if (stack.isStackable()) {
			while (!stack.isEmpty()) {
				if (reverseDirection) {
					if (i < startIndex) {
						break;
					}
				} else if (i >= endIndex) {
					break;
				}

				Slot slot = getSlot(i);
				ItemStack itemstack = slot.getStack();
				if (!itemstack.isEmpty() && areItemsAndTagsEqual(stack, itemstack)) {
					int j = itemstack.getCount() + stack.getCount();
					int maxSize = Math.min(slot.getSlotStackLimit(), stack.getMaxStackSize());
					if (j <= maxSize) {
						stack.setCount(0);
						itemstack.setCount(j);
						slot.onSlotChanged();
						flag = true;
					} else if (itemstack.getCount() < maxSize) {
						stack.shrink(maxSize - itemstack.getCount());
						itemstack.setCount(maxSize);
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

		if (!stack.isEmpty()) {
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

				Slot slot1 = getSlot(i);
				ItemStack itemstack1 = slot1.getStack();
				if (itemstack1.isEmpty() && slot1.isItemValid(stack)) {
					//filter slot logic
					if (slot1 instanceof FilterSlotItemHandler) {
						ItemStack fakeStack = stack.copy();
						fakeStack.setCount(1);
						slot1.putStack(fakeStack);
						//end filter slot logic
					} else if (stack.getCount() > slot1.getSlotStackLimit()) {
						slot1.putStack(stack.split(slot1.getSlotStackLimit()));
					} else {
						slot1.putStack(stack.split(stack.getCount()));
					}

					slot1.onSlotChanged();
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

}
