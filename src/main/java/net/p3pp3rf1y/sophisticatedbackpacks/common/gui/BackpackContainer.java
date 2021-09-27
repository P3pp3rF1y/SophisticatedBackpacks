package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.base.Suppliers;
import com.google.common.collect.Lists;
import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.ints.IntComparators;
import net.minecraft.core.NonNullList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.network.protocol.game.ClientboundContainerSetSlotPacket;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ClickAction;
import net.minecraft.world.inventory.ClickType;
import net.minecraft.world.inventory.ContainerListener;
import net.minecraft.world.inventory.ContainerSynchronizer;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.inventory.ResultSlot;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.minecraftforge.fmllegacy.network.NetworkHooks;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackUpgradeHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.NoopBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackBackgroundProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackContentsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncContainerClientDataMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISlotColorCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort.NoSortSettingsCategory;

import javax.annotation.Nullable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translError;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.BACKPACK_CONTAINER_TYPE;

public class BackpackContainer extends AbstractContainerMenu implements ISyncedContainer {
	public static final ResourceLocation EMPTY_UPGRADE_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_upgrade_slot");
	public static final int NUMBER_OF_PLAYER_SLOTS = 36;
	private static final String OPEN_TAB_ID_TAG = "openTabId";
	private static final String SORT_BY_TAG = "sortBy";
	private static final String UPGRADE_ENABLED_TAG = "upgradeEnabled";
	private static final String UPGRADE_SLOT_TAG = "upgradeSlot";
	private static final String ACTION_TAG = "action";

	private final IBackpackWrapper backpackWrapper;
	private final Player player;
	private int backpackSlotNumber = -1;

	private final BackpackContext backpackContext;

	private final Map<Integer, UpgradeContainerBase<?, ?>> upgradeContainers = new LinkedHashMap<>();
	private Consumer<BackpackContainer> upgradeChangeListener = null;

	private final BackpackBackgroundProperties backpackBackgroundProperties;

	public final NonNullList<ItemStack> lastUpgradeSlots = NonNullList.create();
	public final List<Slot> upgradeSlots = Lists.newArrayList();
	public final NonNullList<ItemStack> remoteUpgradeSlots = NonNullList.create();

	public final NonNullList<ItemStack> lastRealSlots = NonNullList.create();
	public final List<Slot> realInventorySlots = Lists.newArrayList();
	protected final NonNullList<ItemStack> remoteRealSlots = NonNullList.create();

	private final IBackpackWrapper parentBackpackWrapper;

	private final Map<Integer, ItemStack> slotStacksToUpdate = new HashMap<>();

	private boolean isUpdatingFromPacket = false;

	private CompoundTag lastSettingsNbt = null;

	private long errorResultExpirationTime = 0;

	public Optional<UpgradeSlotChangeResult> getErrorUpgradeSlotChangeResult() {
		if (errorUpgradeSlotChangeResult != null && player.level.getGameTime() >= errorResultExpirationTime) {
			errorResultExpirationTime = 0;
			errorUpgradeSlotChangeResult = null;
		}
		return Optional.ofNullable(errorUpgradeSlotChangeResult);
	}

	@Nullable
	private UpgradeSlotChangeResult errorUpgradeSlotChangeResult;

	public BackpackContainer(int windowId, Player player, BackpackContext backpackContext) {
		super(BACKPACK_CONTAINER_TYPE.get(), windowId);
		this.player = player;
		this.backpackContext = backpackContext;
		parentBackpackWrapper = backpackContext.getParentBackpackWrapper(player).orElse(NoopBackpackWrapper.INSTANCE);
		backpackWrapper = backpackContext.getBackpackWrapper(player);
		removeOpenTabIfKeepOff();
		backpackWrapper.fillWithLoot(player);
		backpackBackgroundProperties = (getNumberOfBackpackInventorySlots() + backpackWrapper.getColumnsTaken() * backpackWrapper.getNumberOfSlotRows()) <= 81 ? BackpackBackgroundProperties.REGULAR : BackpackBackgroundProperties.WIDE;

		initSlotsAndContainers(player, backpackContext.getBackpackSlotIndex(), backpackContext.shouldLockBackpackSlot(player));
		backpackWrapper.getContentsUuid().ifPresent(backpackUuid ->
		{
			ItemStack backpack = backpackWrapper.getBackpack();
			BackpackAccessLogger.logPlayerAccess(player, backpack.getItem(), backpackUuid, backpack.getHoverName().getString(),
					backpackWrapper.getClothColor(), backpackWrapper.getBorderColor(), backpackWrapper.getColumnsTaken());
		});
	}

	private void sendBackpackSettingsToClient() {
		if (player.level.isClientSide) {
			return;
		}

		backpackWrapper.getContentsUuid().ifPresent(uuid -> {
			CompoundTag settingsContents = new CompoundTag();
			CompoundTag settingsNbt = backpackWrapper.getSettingsHandler().getNbt();
			if (!settingsNbt.isEmpty()) {
				settingsContents.put(BackpackSettingsHandler.SETTINGS_TAG, settingsNbt);
				PacketHandler.sendToClient((ServerPlayer) player, new BackpackContentsMessage(uuid, settingsContents));
			}
		});
	}

	public IBackpackWrapper getParentBackpackWrapper() {
		return parentBackpackWrapper;
	}

	public int getColumnsTaken() {
		return backpackWrapper.getColumnsTaken();
	}

	private void initSlotsAndContainers(Player player, int backpackSlotIndex, boolean shouldLockBackpackSlot) {
		int yPosition = addBackpackInventorySlots();
		addPlayerInventorySlots(player.getInventory(), yPosition, backpackSlotIndex, shouldLockBackpackSlot);
		addBackpackUpgradeSlots(yPosition);
		addUpgradeSettingsContainers(player);
	}

	private void addUpgradeSettingsContainers(Player player) {
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
		slot.index = getTotalSlotsNumber();
		upgradeSlots.add(slot);
		lastUpgradeSlots.add(ItemStack.EMPTY);
		remoteUpgradeSlots.add(ItemStack.EMPTY);
	}

	protected void addNoSortSlot(Slot slot) {
		slot.index = getInventorySlotsSize();
		realInventorySlots.add(slot);
		lastRealSlots.add(ItemStack.EMPTY);
		remoteRealSlots.add(ItemStack.EMPTY);
	}

	@Override
	protected Slot addSlot(Slot slot) {
		slot.index = getInventorySlotsSize();
		slots.add(slot);
		lastSlots.add(ItemStack.EMPTY);
		remoteSlots.add(ItemStack.EMPTY);
		realInventorySlots.add(slot);
		lastRealSlots.add(ItemStack.EMPTY);
		remoteRealSlots.add(ItemStack.EMPTY);
		return slot;
	}

	public void setUpgradeChangeListener(Consumer<BackpackContainer> upgradeChangeListener) {
		this.upgradeChangeListener = upgradeChangeListener;
	}

	private int addBackpackInventorySlots() {
		BackpackInventoryHandler inventoryHandler = backpackWrapper.getInventoryHandler();
		int slotIndex = 0;
		int yPosition = 18;

		Set<Integer> noSortSlotIndexes = getNoSortSlotIndexes();
		while (slotIndex < inventoryHandler.getSlots()) {
			int lineIndex = slotIndex % getSlotsOnLine();
			int finalSlotIndex = slotIndex;
			BackpackInventorySlot slot = new BackpackInventorySlot(player.level.isClientSide, backpackWrapper, inventoryHandler, finalSlotIndex, lineIndex, yPosition);
			if (noSortSlotIndexes.contains(slotIndex)) {
				addNoSortSlot(slot);
			} else {
				addSlot(slot);
			}

			slotIndex++;
			if (slotIndex % getSlotsOnLine() == 0) {
				yPosition += 18;
			}
		}

		return getNumberOfRows() * 18 + 18;
	}

	private void addPlayerInventorySlots(Inventory playerInventory, int yPosition, int backpackSlotIndex, boolean shouldLockBackpackSlot) {
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
	public void initializeContents(int stateId, List<ItemStack> items, ItemStack carried) {
		backpackWrapper.setPersistent(false);
		isUpdatingFromPacket = true;
		super.initializeContents(stateId, items, carried);
		isUpdatingFromPacket = false;
		backpackWrapper.setPersistent(true);
		backpackWrapper.getInventoryHandler().saveInventory();
		backpackWrapper.getUpgradeHandler().saveInventory();
	}

	private Slot addBackpackSafeSlot(Inventory playerInventory, int yPosition, int slotIndex, int xPosition, int backpackSlotIndex, boolean shouldLockBackpackSlot) {
		Slot slot;
		if (shouldLockBackpackSlot && slotIndex == backpackSlotIndex) {
			slot = new Slot(playerInventory, slotIndex, xPosition, yPosition) {
				@Override
				public boolean mayPickup(Player playerIn) {
					return false;
				}

				@Override
				public void setChanged() {
					super.setChanged();
					closeBackpackScreenIfSomethingMessedWithBackpackStack(getItem());
				}
			};
		} else {
			slot = new Slot(playerInventory, slotIndex, xPosition, yPosition);
		}

		return addSlot(slot);
	}

	public void closeBackpackScreenIfSomethingMessedWithBackpackStack(ItemStack supposedToBeBackpackStack) {
		if (!isClientSide() && isNotCorrectBackpack(supposedToBeBackpackStack)) {
			player.closeContainer();
		}
	}

	private boolean isNotCorrectBackpack(ItemStack supposedToBeBackpackStack) {
		return supposedToBeBackpackStack.isEmpty() || !(supposedToBeBackpackStack.getItem() instanceof BackpackItem) || supposedToBeBackpackStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(w -> w != (isFirstLevelBackpack() ? backpackWrapper : parentBackpackWrapper)).orElse(true);
	}

	private boolean isClientSide() {
		return player.level.isClientSide;
	}

	private void addSlotAndUpdateBackpackSlotNumber(int backpackSlotIndex, boolean lockBackpackSlot, int slotIndex, Slot slot) {
		if (lockBackpackSlot && slotIndex == backpackSlotIndex) {
			backpackSlotNumber = slot.index;
		}
	}

	public int getNumberOfRows() {
		return backpackWrapper.getNumberOfSlotRows();
	}

	public int getSlotsOnLine() {
		return backpackBackgroundProperties.getSlotsOnLine() - backpackWrapper.getColumnsTaken();
	}

	@Override
	public boolean stillValid(Player player) {
		return backpackContext.canInteractWith(player);
	}

	public static BackpackContainer fromBuffer(int windowId, Inventory playerInventory, FriendlyByteBuf packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, BackpackContext.fromBuffer(packetBuffer));
	}

	@Override
	public ItemStack quickMoveStack(Player playerIn, int index) {
		ItemStack itemstack = ItemStack.EMPTY;
		Slot slot = getSlot(index);
		if (slot.hasItem()) {
			Optional<UpgradeContainerBase<?, ?>> upgradeContainer = getSlotUpgradeContainer(slot);
			ItemStack slotStack = upgradeContainer.map(c -> c.getSlotStackToTransfer(slot)).orElse(slot.getItem());
			itemstack = slotStack.copy();

			if (!mergeSlotStack(slot, index, slotStack)) {
				return ItemStack.EMPTY;
			}

			if (slotStack.isEmpty()) {
				slot.set(ItemStack.EMPTY);
			} else {
				slot.setChanged();
			}
			slot.onQuickCraft(slotStack, itemstack);

			if (upgradeContainer.isPresent()) {
				upgradeContainer.ifPresent(c -> c.onTakeFromSlot(slot, player, slotStack));
			} else {
				slot.onTake(player, slotStack);
			}
		}

		return itemstack;
	}

	private boolean mergeSlotStack(Slot slot, int index, ItemStack slotStack) {
		if (isUpgradeSlot(index)) {
			return mergeStackToBackpack(slotStack) || mergeStackToPlayersInventory(slotStack);
		} else if (isBackpackInventorySlot(index)) {
			if (shouldShiftClickIntoOpenTabFirst()) {
				return mergeStackToOpenUpgradeTab(slotStack) || mergeStackToPlayersInventory(slotStack);
			}
			return mergeStackToPlayersInventory(slotStack) || mergeStackToOpenUpgradeTab(slotStack);
		} else if (isUpgradeSettingsSlot(index)) {
			if (getSlotUpgradeContainer(slot).map(c -> c.mergeIntoBackpackFirst(slot)).orElse(true)) {
				return mergeStackToBackpack(slotStack) || mergeStackToPlayersInventory(slotStack);
			}
			return mergeStackToPlayersInventory(slotStack) || mergeStackToBackpack(slotStack);
		} else {
			if (shouldShiftClickIntoOpenTabFirst()) {
				return mergeStackToOpenUpgradeTab(slotStack) || mergeStackToUpgradeSlots(slotStack) || mergeStackToBackpack(slotStack);
			}
			return mergeStackToUpgradeSlots(slotStack) || mergeStackToBackpack(slotStack) || mergeStackToOpenUpgradeTab(slotStack);
		}
	}

	private boolean shouldShiftClickIntoOpenTabFirst() {
		return BackpackSettingsManager.getBackpackSettingValue(player, backpackWrapper.getSettingsHandler().getTypeCategory(BackpackSettingsCategory.class), BackpackSettingsManager.SHIFT_CLICK_INTO_OPEN_TAB_FIRST);
	}

	private boolean mergeStackToUpgradeSlots(ItemStack slotStack) {
		return !upgradeSlots.isEmpty() && moveItemStackTo(slotStack, getInventorySlotsSize(), getInventorySlotsSize() + getNumberOfUpgradeSlots(), false);
	}

	public int getInventorySlotsSize() {
		return realInventorySlots.size();
	}

	private boolean mergeStackToOpenUpgradeTab(ItemStack slotStack) {
		return getOpenContainer().map(c -> {
			List<Slot> slots = c.getSlots();
			if (slots.isEmpty()) {
				return false;
			}
			int firstSlotIndex = slots.get(0).index;
			int lastSlotIndex = slots.get(slots.size() - 1).index;
			return mergeItemStack(slotStack, firstSlotIndex, lastSlotIndex + 1, false, true);
		}).orElse(false);
	}

	private boolean mergeStackToBackpack(ItemStack slotStack) {
		return moveItemStackTo(slotStack, 0, getNumberOfBackpackInventorySlots(), false);
	}

	private boolean mergeStackToPlayersInventory(ItemStack slotStack) {
		return mergeItemStack(slotStack, getNumberOfBackpackInventorySlots(), getInventorySlotsSize(), true, true);
	}

	public boolean isPlayersInventorySlot(int slotNumber) {
		return slotNumber >= getNumberOfBackpackInventorySlots() && slotNumber < getInventorySlotsSize();
	}

	private boolean isUpgradeSettingsSlot(int index) {
		return index >= getNumberOfBackpackInventorySlots() + getNumberOfUpgradeSlots() + NUMBER_OF_PLAYER_SLOTS;
	}

	private boolean isBackpackInventorySlot(int index) {
		return index < getNumberOfBackpackInventorySlots();
	}

	private boolean isUpgradeSlot(int index) {
		return index >= getFirstUpgradeSlot() && (index - getFirstUpgradeSlot() < getNumberOfUpgradeSlots());
	}

	public int getFirstUpgradeSlot() {
		return getInventorySlotsSize();
	}

	public Optional<UpgradeContainerBase<?, ?>> getSlotUpgradeContainer(Slot slot) {
		if (isUpgradeSettingsSlot(slot.index)) {
			for (UpgradeContainerBase<?, ?> upgradeContainer : upgradeContainers.values()) {
				if (upgradeContainer.containsSlot(slot)) {
					return Optional.of(upgradeContainer);
				}
			}
		}
		return Optional.empty();
	}

	@Override
	public void clicked(int slotId, int dragType, ClickType clickType, Player player) {
		if (isUpgradeSettingsSlot(slotId) && getSlot(slotId) instanceof IFilterSlot && getSlot(slotId).mayPlace(getCarried())) {
			Slot slot = getSlot(slotId);
			ItemStack cursorStack = getCarried().copy();
			if (cursorStack.getCount() > 1) {
				cursorStack.setCount(1);
			}

			slot.set(cursorStack);
			return;
		} else if (isUpgradeSlot(slotId) && getSlot(slotId) instanceof BackpackUpgradeSlot) {
			Slot slot = getSlot(slotId);
			ItemStack slotStack = slot.getItem();
			if (slot.mayPlace(getCarried())) {
				BackpackUpgradeSlot upgradeSlot = (BackpackUpgradeSlot) slot;
				ItemStack cursorStack = getCarried();
				IBackpackUpgradeItem<?> backpackUpgradeItem = (IBackpackUpgradeItem<?>) cursorStack.getItem();
				int newColumnsTaken = backpackUpgradeItem.getInventoryColumnsTaken();
				int currentColumnsTaken = 0;
				if (!slotStack.isEmpty()) {
					currentColumnsTaken = ((IBackpackUpgradeItem<?>) slotStack.getItem()).getInventoryColumnsTaken();
				}
				if (needsSlotsThatAreOccupied(cursorStack, currentColumnsTaken, upgradeSlot, newColumnsTaken)) {
					return;
				}

				int columnsToRemove = newColumnsTaken - currentColumnsTaken;
				if (slotStack.isEmpty() || upgradeSlot.canSwapStack(player, cursorStack)) {
					setCarried(slotStack);
					upgradeSlot.set(cursorStack);
					updateColumnsTaken(columnsToRemove);
					upgradeSlot.setChanged();
				}
			} else if ((getCarried().isEmpty() || slot.mayPlace(getCarried())) && !slotStack.isEmpty() && slot.mayPickup(player)) {
				int k2 = dragType == 0 ? Math.min(slotStack.getCount(), slotStack.getMaxStackSize()) : Math.min(slotStack.getMaxStackSize() + 1, slotStack.getCount() + 1) / 2;
				int columnsTaken = ((IBackpackUpgradeItem<?>) slotStack.getItem()).getInventoryColumnsTaken();
				if (clickType == ClickType.QUICK_MOVE) {
					quickMoveStack(player, slotId);
				} else {
					setCarried(slot.remove(k2));
				}
				updateColumnsTaken(-columnsTaken);
				slot.onTake(player, getCarried());
			}
			return;
		}

		super.clicked(slotId, dragType, clickType, player);
	}

	private void updateColumnsTaken(int columnsToRemove) {
		if (columnsToRemove != 0) {
			backpackWrapper.setColumnsTaken(Math.max(0, backpackWrapper.getColumnsTaken() + columnsToRemove));
			backpackWrapper.onContentsNbtUpdated();
			refreshAllSlots();
		}
	}

	private boolean needsSlotsThatAreOccupied(ItemStack cursorStack, int currentColumnsTaken, BackpackUpgradeSlot upgradeSlot, int newColumnsTaken) {
		if (currentColumnsTaken >= newColumnsTaken) {
			return false;
		}

		int slotsToCheck = (newColumnsTaken - currentColumnsTaken) * getNumberOfRows();

		BackpackInventoryHandler invHandler = backpackWrapper.getInventoryHandler();
		Set<Integer> errorSlots = new HashSet<>();
		int slots = getNumberOfBackpackInventorySlots();
		for (int slotIndex = slots - 1; slotIndex >= slots - slotsToCheck; slotIndex--) {
			if (!invHandler.getStackInSlot(slotIndex).isEmpty()) {
				errorSlots.add(slotIndex);
			}
		}

		if (!errorSlots.isEmpty()) {
			upgradeSlot.updateSlotChangeError(new UpgradeSlotChangeResult.Fail(translError("add.needs_occupied_inventory_slots", slotsToCheck, cursorStack.getHoverName()), Collections.emptySet(), errorSlots, Collections.emptySet()));
			return true;
		}
		return false;
	}

	public int getNumberOfBackpackInventorySlots() {
		return backpackWrapper.getInventoryHandler().getSlots();
	}

	public BackpackBackgroundProperties getBackpackBackgroundProperties() {
		return backpackBackgroundProperties;
	}

	public int getNumberOfUpgradeSlots() {
		return backpackWrapper.getUpgradeHandler().getSlots();
	}

	public int getFirstUpgradeSettingsSlot() {
		return getNumberOfBackpackInventorySlots() + getNumberOfUpgradeSlots() + NUMBER_OF_PLAYER_SLOTS;
	}

	public Map<Integer, UpgradeContainerBase<?, ?>> getUpgradeContainers() {
		return upgradeContainers;
	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (data.contains("containerId")) {
			int containerId = data.getInt("containerId");
			if (upgradeContainers.containsKey(containerId)) {
				upgradeContainers.get(containerId).handleMessage(data);
			}
		} else if (data.contains(OPEN_TAB_ID_TAG)) {
			setOpenTabId(data.getInt(OPEN_TAB_ID_TAG));
		} else if (data.contains(SORT_BY_TAG)) {
			setSortBy(SortBy.fromName(data.getString(SORT_BY_TAG)));
		} else if (data.contains(ACTION_TAG)) {
			String actionName = data.getString(ACTION_TAG);
			switch (actionName) {
				case "sort" -> sort();
				case "openSettings" -> openSettings();
				default -> {
				}
			}
		} else if (data.contains(UPGRADE_ENABLED_TAG)) {
			setUpgradeEnabled(data.getInt(UPGRADE_SLOT_TAG), data.getBoolean(UPGRADE_ENABLED_TAG));
		}
	}

	public void setOpenTabId(int tabId) {
		if (isClientSide()) {
			sendToServer(data -> data.putInt(OPEN_TAB_ID_TAG, tabId));
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

	private void sendToServer(Consumer<CompoundTag> addData) {
		CompoundTag data = new CompoundTag();
		addData.accept(data);
		PacketHandler.sendToServer(new SyncContainerClientDataMessage(data));
	}

	public void setSortBy(SortBy sortBy) {
		if (isClientSide()) {
			sendToServer(data -> data.putString(SORT_BY_TAG, sortBy.getSerializedName()));
		}
		backpackWrapper.setSortBy(sortBy);
	}

	public void sort() {
		if (isClientSide()) {
			sendToServer(data -> data.putString(ACTION_TAG, "sort"));
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
			sendToServer(data -> {
				data.putBoolean(UPGRADE_ENABLED_TAG, enabled);
				data.putInt(UPGRADE_SLOT_TAG, upgradeSlot);
			});
		}
		slotWrappers.get(upgradeSlot).setEnabled(enabled);
	}

	public Optional<UpgradeContainerBase<?, ?>> getOpenContainer() {
		return backpackWrapper.getOpenTabId().flatMap(id -> upgradeContainers.containsKey(id) ? Optional.of(upgradeContainers.get(id)) : Optional.empty());
	}

	public void openSettings() {
		if (isClientSide()) {
			sendToServer(data -> data.putString(ACTION_TAG, "openSettings"));
			return;
		}
		NetworkHooks.openGui((ServerPlayer) player, new SimpleMenuProvider((w, p, pl) -> new SettingsContainer(w, pl, backpackContext),
				new TranslatableComponent(TranslationHelper.translGui("settings.title"))), backpackContext::toBuffer);
	}

	public List<Integer> getSlotOverlayColors(int slot) {
		List<Integer> ret = new ArrayList<>();
		backpackWrapper.getSettingsHandler().getCategoriesThatImplement(ISlotColorCategory.class).forEach(c -> c.getSlotColor(slot).ifPresent(ret::add));
		return ret;
	}

	public int getUpgradeSlotsSize() {
		return upgradeSlots.size();
	}

	public class BackpackUpgradeSlot extends SlotItemHandler {
		private boolean wasEmpty = false;

		public BackpackUpgradeSlot(BackpackUpgradeHandler upgradeHandler, int slotIndex, int yPosition) {
			super(upgradeHandler, slotIndex, -15, yPosition);
		}

		@Override
		public void setChanged() {
			super.setChanged();
			if ((!isUpdatingFromPacket && wasEmpty != getItem().isEmpty()) || updateWrappersAndCheckForReloadNeeded()) {
				reloadUpgradeControl();
				if (!isFirstLevelBackpack()) {
					parentBackpackWrapper.getUpgradeHandler().refreshUpgradeWrappers();
				}
				backpackContext.onUpgradeChanged(player);
			}
			wasEmpty = getItem().isEmpty();
		}

		@Override
		public boolean mayPlace(ItemStack stack) {
			if (stack.isEmpty() || !(stack.getItem() instanceof IBackpackUpgradeItem)) {
				return false;
			}
			UpgradeSlotChangeResult result = ((IBackpackUpgradeItem<?>) stack.getItem()).canAddUpgradeTo(backpackWrapper, stack, isFirstLevelBackpack());
			updateSlotChangeError(result);

			return result.isSuccessful();
		}

		private void updateSlotChangeError(UpgradeSlotChangeResult result) {
			if (player.level.isClientSide && !result.isSuccessful()) {
				errorUpgradeSlotChangeResult = result;
				errorResultExpirationTime = player.level.getGameTime() + 60;
			}
		}

		@Override
		public boolean mayPickup(Player player) {
			boolean ret = super.mayPickup(player);
			if (!ret) {
				return false;
			}

			UpgradeSlotChangeResult result = ((IBackpackUpgradeItem<?>) getItem().getItem()).canRemoveUpgradeFrom(backpackWrapper);
			updateSlotChangeError(result);
			return result.isSuccessful();
		}

		public boolean canSwapStack(Player player, ItemStack stackToPut) {
			boolean ret = super.mayPickup(player);
			if (!ret) {
				return false;
			}
			UpgradeSlotChangeResult result = ((IBackpackUpgradeItem<?>) getItem().getItem()).canSwapUpgradeFor(stackToPut, backpackWrapper);
			updateSlotChangeError(result);
			return result.isSuccessful();
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
					int upgradeSlotIndex = slot.index - getInventorySlotsSize();
					slotNumbersToRemove.add(upgradeSlotIndex);
					upgradeSlots.remove(slot);
				});
			}
			slotNumbersToRemove.sort(IntComparators.OPPOSITE_COMPARATOR);
			for (int slotNumber : slotNumbersToRemove) {
				lastUpgradeSlots.remove(slotNumber);
				remoteUpgradeSlots.remove(slotNumber);
			}
		}

		private void onUpgradesChanged() {
			if (upgradeChangeListener != null) {
				upgradeChangeListener.accept(BackpackContainer.this);
			}
		}

		@Nullable
		@Override
		public Pair<ResourceLocation, ResourceLocation> getNoItemIcon() {
			return new Pair<>(InventoryMenu.BLOCK_ATLAS, EMPTY_UPGRADE_SLOT_BACKGROUND);
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
	public NonNullList<ItemStack> getItems() {
		NonNullList<ItemStack> list = NonNullList.create();

		realInventorySlots.forEach(slot -> list.add(slot.getItem()));
		upgradeSlots.forEach(upgradeSlot -> list.add(upgradeSlot.getItem()));
		return list;
	}

	public int getTotalSlotsNumber() {
		return getInventorySlotsSize() + upgradeSlots.size();
	}

	@Override
	public void broadcastChanges() {
		if (backpackSlotNumber != -1) {
			closeBackpackScreenIfSomethingMessedWithBackpackStack(getSlot(backpackSlotNumber).getItem());
		}

		synchronizeCarriedToRemote();
		broadcastChangesIn(lastUpgradeSlots, remoteUpgradeSlots, upgradeSlots, getFirstUpgradeSlot());
		broadcastChangesIn(lastRealSlots, remoteRealSlots, realInventorySlots, 0);
		if (lastSettingsNbt == null || !lastSettingsNbt.equals(backpackWrapper.getSettingsHandler().getNbt())) {
			lastSettingsNbt = backpackWrapper.getSettingsHandler().getNbt().copy();
			sendBackpackSettingsToClient();
			refreshInventorySlotsIfNeeded();
		}
	}

	@Override
	public void broadcastFullState() {
		broadcastFullStateOf(lastUpgradeSlots, upgradeSlots, getFirstUpgradeSlot());
		broadcastFullStateOf(lastRealSlots, realInventorySlots, 0);

		sendAllDataToRemote();
	}

	private void broadcastFullStateOf(NonNullList<ItemStack> lastSlotsCollection, List<Slot> slotsCollection, int slotIndexOffset) {
		for (int i = 0; i < slotsCollection.size(); ++i) {
			ItemStack itemstack = slotsCollection.get(i).getItem();
			triggerSlotListeners(i, itemstack, itemstack::copy, lastSlotsCollection, slotIndexOffset);
		}
	}

	private void broadcastChangesIn(NonNullList<ItemStack> lastSlotsCollection, NonNullList<ItemStack> remoteSlotsCollection, List<Slot> slotsCollection, int slotIndexOffset) {
		for (int i = 0; i < slotsCollection.size(); ++i) {
			ItemStack itemstack = slotsCollection.get(i).getItem();
			Supplier<ItemStack> supplier = Suppliers.memoize(itemstack::copy);
			triggerSlotListeners(i, itemstack, supplier, lastSlotsCollection, slotIndexOffset);
			synchronizeSlotToRemote(i, itemstack, supplier, remoteSlotsCollection, slotIndexOffset);
		}
	}

	private void triggerSlotListeners(int stackIndex, ItemStack slotStack, Supplier<ItemStack> slotStackCopy, NonNullList<ItemStack> lastSlotsCollection, int slotIndexOffset) {
		ItemStack itemstack = lastSlotsCollection.get(stackIndex);
		if (!ItemStack.matches(itemstack, slotStack)) {
			boolean clientStackChanged = !slotStack.equals(itemstack, true);
			ItemStack stackCopy = slotStackCopy.get();
			lastSlotsCollection.set(stackIndex, stackCopy);

			if (clientStackChanged) {
				for (ContainerListener containerlistener : containerListeners) {
					containerlistener.slotChanged(this, stackIndex + slotIndexOffset, stackCopy);
				}
			}
		}
	}

	private void synchronizeSlotToRemote(int slotIndex, ItemStack slotStack, Supplier<ItemStack> slotStackCopy, NonNullList<ItemStack> remoteSlotsCollection, int slotIndexOffset) {
		if (!suppressRemoteUpdates) {
			ItemStack itemstack = remoteSlotsCollection.get(slotIndex);
			if (!ItemStack.matches(itemstack, slotStack)) {
				ItemStack stackCopy = slotStackCopy.get();
				remoteSlotsCollection.set(slotIndex, stackCopy);
				if (synchronizer != null) {
					synchronizer.sendSlotChange(this, slotIndex + slotIndexOffset, stackCopy);
				}
			}
		}
	}

	private void refreshInventorySlotsIfNeeded() {
		Set<Integer> noSortSlotIndexes = getNoSortSlotIndexes();
		boolean needRefresh = false;
		if (realInventorySlots.size() - slots.size() != noSortSlotIndexes.size()) {
			needRefresh = true;
		} else {
			for (Slot slot : realInventorySlots) {
				if (!slots.contains(slot) && !noSortSlotIndexes.contains(slot.index)) {
					needRefresh = true;
					break;
				}
			}
		}

		if (!needRefresh) {
			return;
		}

		slots.clear();
		lastSlots.clear();
		realInventorySlots.clear();
		lastRealSlots.clear();
		remoteRealSlots.clear();
		int yPosition = addBackpackInventorySlots();
		addPlayerInventorySlots(player.getInventory(), yPosition, backpackContext.getBackpackSlotIndex(), backpackContext.shouldLockBackpackSlot(player));
	}

	private void refreshAllSlots() {
		slots.clear();
		lastSlots.clear();
		realInventorySlots.clear();
		lastRealSlots.clear();
		remoteRealSlots.clear();
		upgradeSlots.clear();
		lastUpgradeSlots.clear();
		remoteUpgradeSlots.clear();
		upgradeContainers.clear();

		initSlotsAndContainers(player, backpackContext.getBackpackSlotIndex(), backpackContext.shouldLockBackpackSlot(player));
	}

	private Set<Integer> getNoSortSlotIndexes() {
		return backpackWrapper.getSettingsHandler().getTypeCategory(NoSortSettingsCategory.class).getNoSortSlots();
	}

	public void detectSettingsChangeAndReload() {
		backpackWrapper.getContentsUuid().ifPresent(uuid -> {
			BackpackStorage storage = BackpackStorage.get();
			if (storage.removeUpdatedBackpackSettingsFlag(uuid)) {
				backpackWrapper.getSettingsHandler().reloadFrom(storage.getOrCreateBackpackContents(uuid));
				refreshInventorySlotsIfNeeded();
			}
		});
	}

	@Override
	public Slot getSlot(int slotId) {
		if (slotId >= getInventorySlotsSize()) {
			return upgradeSlots.get(slotId - getInventorySlotsSize());
		} else {
			return realInventorySlots.get(slotId);
		}
	}

	private static final Method ON_SWAP_CRAFT = ObfuscationReflectionHelper.findMethod(Slot.class, "m_6405_", int.class);

	private void onSwapCraft(Slot slot, int numItemsCrafted) {
		try {
			ON_SWAP_CRAFT.invoke(slot, numItemsCrafted);
		}
		catch (IllegalAccessException | InvocationTargetException e) {
			SophisticatedBackpacks.LOGGER.error("Error invoking onSwapCraft method in Slot class", e);
		}
	}

	//copy of Container's doClick with the replacement of inventorySlots.get to getSlot, call to onswapcraft as that's protected in vanilla and an addition of upgradeSlots to pickup all
	@SuppressWarnings("java:S3776")
	//complexity here is brutal, but it's something that's in vanilla and need to keep this as close to it as possible for easier ports
	@Override
	protected void doClick(int slotId, int dragType, ClickType p_150433_, Player p_150434_) {
		Inventory inventory = p_150434_.getInventory();
		if (p_150433_ == ClickType.QUICK_CRAFT) {
			int i = quickcraftStatus;
			quickcraftStatus = getQuickcraftHeader(dragType);
			if ((i != 1 || quickcraftStatus != 2) && i != quickcraftStatus) {
				resetQuickCraft();
			} else if (getCarried().isEmpty()) {
				resetQuickCraft();
			} else if (quickcraftStatus == 0) {
				quickcraftType = getQuickcraftType(dragType);
				if (isValidQuickcraftType(quickcraftType, p_150434_)) {
					quickcraftStatus = 1;
					quickcraftSlots.clear();
				} else {
					resetQuickCraft();
				}
			} else if (quickcraftStatus == 1) {
				Slot slot = getSlot(slotId);
				ItemStack itemstack = getCarried();
				if (canItemQuickReplace(slot, itemstack) && slot.mayPlace(itemstack) && (quickcraftType == 2 || itemstack.getCount() > quickcraftSlots.size()) && canDragTo(slot)) {
					quickcraftSlots.add(slot);
				}
			} else if (quickcraftStatus == 2) {
				if (!quickcraftSlots.isEmpty()) {
					if (quickcraftSlots.size() == 1) {
						int l = (quickcraftSlots.iterator().next()).index;
						resetQuickCraft();
						doClick(l, quickcraftType, ClickType.PICKUP, p_150434_);
						return;
					}

					ItemStack carried = getCarried().copy();
					int j1 = getCarried().getCount();

					for (Slot slot1 : quickcraftSlots) {
						ItemStack itemstack1 = getCarried();
						if (slot1 != null && canItemQuickReplace(slot1, itemstack1) && slot1.mayPlace(itemstack1) && (quickcraftType == 2 || itemstack1.getCount() >= quickcraftSlots.size()) && canDragTo(slot1)) {
							ItemStack carriedCopy = carried.copy();
							int j = slot1.hasItem() ? slot1.getItem().getCount() : 0;
							getQuickCraftSlotCount(quickcraftSlots, quickcraftType, carriedCopy, j);
							int slotStackLimit = slot1.getMaxStackSize(carriedCopy);
							if (!(slot1 instanceof BackpackInventorySlot) && slotStackLimit > carriedCopy.getMaxStackSize()) {
								slotStackLimit = carriedCopy.getMaxStackSize();
							}
							if (carriedCopy.getCount() > slotStackLimit) {
								carriedCopy.setCount(slotStackLimit);
							}

							j1 -= carriedCopy.getCount() - j;
							slot1.set(carriedCopy);
						}
					}

					carried.setCount(j1);
					setCarried(carried);
				}

				resetQuickCraft();
			} else {
				resetQuickCraft();
			}
		} else if (quickcraftStatus != 0) {
			resetQuickCraft();
		} else if ((p_150433_ == ClickType.PICKUP || p_150433_ == ClickType.QUICK_MOVE) && (dragType == 0 || dragType == 1)) {
			ClickAction clickaction = dragType == 0 ? ClickAction.PRIMARY : ClickAction.SECONDARY;
			if (slotId == -999) {
				if (!getCarried().isEmpty()) {
					if (clickaction == ClickAction.PRIMARY) {
						p_150434_.drop(getCarried(), true);
						setCarried(ItemStack.EMPTY);
					} else {
						p_150434_.drop(getCarried().split(1), true);
					}
				}
			} else if (p_150433_ == ClickType.QUICK_MOVE) {
				if (slotId < 0) {
					return;
				}

				Slot slot6 = getSlot(slotId);
				if (!slot6.mayPickup(p_150434_)) {
					return;
				}

				if (isBackpackInventorySlot(slotId)) {
					quickMoveStack(player, slotId).copy();
				} else {
					ItemStack itemstack8 = quickMoveStack(player, slotId);
					while (!itemstack8.isEmpty() && ItemStack.isSame(slot6.getItem(), itemstack8)) {
						itemstack8 = quickMoveStack(player, slotId);
					}
				}
			} else {
				if (slotId < 0) {
					return;
				}

				Slot slot7 = getSlot(slotId);
				ItemStack slotStack = slot7.getItem();
				ItemStack carriedStack = getCarried();
				p_150434_.updateTutorialInventoryAction(carriedStack, slot7.getItem(), clickaction);
				if (!carriedStack.overrideStackedOnOther(slot7, clickaction, p_150434_) && !slotStack.overrideOtherStackedOnMe(carriedStack, slot7, clickaction, p_150434_, createCarriedSlotAccess())) {
					if (slotStack.isEmpty()) {
						if (!carriedStack.isEmpty()) {
							int l2 = clickaction == ClickAction.PRIMARY ? carriedStack.getCount() : 1;
							setCarried(slot7.safeInsert(carriedStack, l2));
						}
					} else if (slot7.mayPickup(p_150434_)) {
						if (carriedStack.isEmpty()) {
							int i3 = clickaction == ClickAction.PRIMARY ? Math.min(slotStack.getCount(), slotStack.getMaxStackSize()) : Math.min(slotStack.getMaxStackSize() + 1, slotStack.getCount() + 1) / 2;
							Optional<ItemStack> optional1 = slot7.tryRemove(i3, Integer.MAX_VALUE, p_150434_);
							optional1.ifPresent((p_150421_) -> {
								setCarried(p_150421_);
								slot7.onTake(p_150434_, p_150421_);
							});
						} else if (slot7.mayPlace(carriedStack)) {
							if (ItemStack.isSameItemSameTags(slotStack, carriedStack)) {
								int j3 = clickaction == ClickAction.PRIMARY ? carriedStack.getCount() : 1;
								setCarried(slot7.safeInsert(carriedStack, j3));
							} else if (carriedStack.getCount() <= slot7.getMaxStackSize(carriedStack)) {
								slot7.set(carriedStack);
								setCarried(slotStack);
							}
						} else if (ItemStack.isSameItemSameTags(slotStack, carriedStack)) {
							Optional<ItemStack> optional = slot7.tryRemove(slotStack.getCount(), carriedStack.getMaxStackSize() - carriedStack.getCount(), p_150434_);
							optional.ifPresent((p_150428_) -> {
								carriedStack.grow(p_150428_.getCount());
								slot7.onTake(p_150434_, p_150428_);
							});
						}
					}
				}

				slot7.setChanged();
			}
		} else if (p_150433_ == ClickType.SWAP) {
			Slot slot2 = getSlot(slotId);
			ItemStack itemstack4 = inventory.getItem(dragType);
			ItemStack slotStack = slot2.getItem();
			if (!itemstack4.isEmpty() || !slotStack.isEmpty()) {
				if (itemstack4.isEmpty()) {
					if (slot2.mayPickup(p_150434_)) {
						if (slotStack.getCount() <= slotStack.getMaxStackSize()) {
							inventory.setItem(dragType, slotStack);
							onSwapCraft(slot2, slotStack.getCount());
							slot2.set(ItemStack.EMPTY);
							slot2.onTake(p_150434_, slotStack);
						} else {
							inventory.setItem(dragType, slotStack.split(slotStack.getMaxStackSize()));
							slot2.setChanged();
						}
					}
				} else if (slotStack.isEmpty()) {
					if (slot2.mayPlace(itemstack4)) {
						int l1 = slot2.getMaxStackSize(itemstack4);
						if (itemstack4.getCount() > l1) {
							slot2.set(itemstack4.split(l1));
						} else {
							slot2.set(itemstack4);
							inventory.setItem(dragType, ItemStack.EMPTY);
						}
					}
				} else if (slotStack.getCount() <= slotStack.getMaxStackSize() && slot2.mayPickup(p_150434_) && slot2.mayPlace(itemstack4)) {
					int i2 = slot2.getMaxStackSize(itemstack4);
					if (itemstack4.getCount() > i2) {
						slot2.set(itemstack4.split(i2));
						slot2.onTake(p_150434_, slotStack);
						if (!inventory.add(slotStack)) {
							p_150434_.drop(slotStack, true);
						}
					} else {
						slot2.set(itemstack4);
						inventory.setItem(dragType, slotStack);
						slot2.onTake(p_150434_, slotStack);
					}
				}
			}
		} else if (p_150433_ == ClickType.CLONE && p_150434_.getAbilities().instabuild && getCarried().isEmpty() && slotId >= 0) {
			Slot slot5 = getSlot(slotId);
			if (slot5.hasItem()) {
				ItemStack itemstack6 = slot5.getItem().copy();
				itemstack6.setCount(itemstack6.getMaxStackSize());
				setCarried(itemstack6);
			}
		} else if (p_150433_ == ClickType.THROW && getCarried().isEmpty() && slotId >= 0) {
			Slot slot4 = getSlot(slotId);
			int i1 = dragType == 0 ? 1 : slot4.getItem().getCount();
			ItemStack itemstack8 = slot4.safeTake(i1, slot4.getItem().getMaxStackSize(), p_150434_);
			p_150434_.drop(itemstack8, true);
		} else if (p_150433_ == ClickType.PICKUP_ALL && slotId >= 0) {
			Slot slot3 = getSlot(slotId);
			ItemStack carriedStack = getCarried();
			if (!carriedStack.isEmpty() && (!slot3.hasItem() || !slot3.mayPickup(p_150434_))) {
				int k1 = dragType == 0 ? 0 : getInventorySlotsSize() - 1;
				int j2 = dragType == 0 ? 1 : -1;

				for (int k2 = 0; k2 < 2; ++k2) {
					for (int k3 = k1; k3 >= 0 && k3 < getInventorySlotsSize() && carriedStack.getCount() < carriedStack.getMaxStackSize(); k3 += j2) {
						Slot slot8 = getSlot(k3);
						if (slot8.hasItem() && canItemQuickReplace(slot8, carriedStack) && slot8.mayPickup(p_150434_) && canTakeItemForPickAll(carriedStack, slot8)) {
							ItemStack itemstack12 = slot8.getItem();
							if (k2 != 0 || itemstack12.getCount() != itemstack12.getMaxStackSize()) {
								ItemStack itemstack13 = slot8.safeTake(itemstack12.getCount(), carriedStack.getMaxStackSize() - carriedStack.getCount(), p_150434_);
								carriedStack.grow(itemstack13.getCount());
							}
						}
					}
				}

				k1 = dragType == 0 ? 0 : upgradeSlots.size() - 1;

				for (int j = 0; j < 2; ++j) {
					for (int upgradeSlotId = k1; upgradeSlotId >= 0 && upgradeSlotId < upgradeSlots.size() && carriedStack.getCount() < carriedStack.getMaxStackSize(); upgradeSlotId += j2) {
						Slot upgradeSlot = upgradeSlots.get(upgradeSlotId);
						if (upgradeSlot.hasItem() && canItemQuickReplace(upgradeSlot, carriedStack) && upgradeSlot.mayPickup(player) && canTakeItemForPickAll(carriedStack, upgradeSlot)) {
							ItemStack itemstack3 = upgradeSlot.getItem();
							if (j != 0 || itemstack3.getCount() != itemstack3.getMaxStackSize()) {
								int l = Math.min(carriedStack.getMaxStackSize() - carriedStack.getCount(), itemstack3.getCount());
								ItemStack itemstack4 = upgradeSlot.remove(l);
								carriedStack.grow(l);
								if (itemstack4.isEmpty()) {
									upgradeSlot.set(ItemStack.EMPTY);
								}

								upgradeSlot.onTake(player, itemstack4);
							}
						}
					}
				}
			}
		}

		sendSlotUpdates();
	}

	public void sendSlotUpdates() {
		if (!player.level.isClientSide) {
			ServerPlayer serverPlayer = (ServerPlayer) player;
			slotStacksToUpdate.forEach((slot, stack) -> serverPlayer.connection.send(new ClientboundContainerSetSlotPacket(serverPlayer.containerMenu.containerId, incrementStateId(), slot, stack)));
			slotStacksToUpdate.clear();
		}
	}

	public static boolean canItemQuickReplace(@Nullable Slot slot, ItemStack stack) {
		boolean flag = slot == null || !slot.hasItem();
		if (!flag && stack.sameItem(slot.getItem()) && ItemStack.tagMatches(slot.getItem(), stack)) {
			return slot.getItem().getCount() <= calculateMaxCountForStack(slot.getMaxStackSize(), stack);
		} else {
			return flag;
		}
	}

	private static int calculateMaxCountForStack(int slotLimit, ItemStack stack) {
		return slotLimit / 64 * stack.getMaxStackSize();
	}

	@Override
	protected boolean moveItemStackTo(ItemStack stack, int startIndex, int endIndex, boolean reverseDirection) {
		return mergeItemStack(stack, startIndex, endIndex, reverseDirection, false);
	}

	//copy of mergeItemStack from Container - just calling getSlot here to account for upgrade slots instead of direct inventorySlots.get
	// and minor addition to be able to ignore magetslotx stack size
	@SuppressWarnings({"java:S3776", "java:S135"})
	//need to keep this very close to vanilla for easy port so not refactoring it to lower complexity or less exit points in loops
	protected boolean mergeItemStack(ItemStack sourceStack, int startIndex, int endIndex, boolean reverseDirection, boolean transferMaxStackSizeFromSource) {
		boolean flag = false;
		int i = startIndex;
		if (reverseDirection) {
			i = endIndex - 1;
		}

		int toTransfer = transferMaxStackSizeFromSource ? Math.min(sourceStack.getMaxStackSize(), sourceStack.getCount()) : sourceStack.getCount();
		if (sourceStack.isStackable() || getSlot(startIndex).getMaxStackSize() > 64) {
			while (toTransfer > 0) {
				if (reverseDirection) {
					if (i < startIndex) {
						break;
					}
				} else if (i >= endIndex) {
					break;
				}

				Slot slot = getSlot(i);
				ItemStack destStack = slot.getItem();
				if (!destStack.isEmpty() && ItemStack.isSameItemSameTags(sourceStack, destStack)) {
					int j = destStack.getCount() + toTransfer;
					int maxSize = calculateMaxCountForStack(slot.getMaxStackSize(), sourceStack);
					if (j <= maxSize) {
						sourceStack.shrink(toTransfer);
						destStack.setCount(j);
						toTransfer = 0;
						slot.setChanged();
						flag = true;
					} else if (destStack.getCount() < maxSize) {
						sourceStack.shrink(maxSize - destStack.getCount());
						toTransfer -= maxSize - destStack.getCount();
						destStack.setCount(maxSize);
						slot.setChanged();
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

				Slot destSlot = getSlot(i);
				ItemStack itemstack1 = destSlot.getItem();
				if (itemstack1.isEmpty() && destSlot.mayPlace(sourceStack) && !(destSlot instanceof IFilterSlot)) {
					if (toTransfer > destSlot.getMaxStackSize()) {
						destSlot.set(sourceStack.split(destSlot.getMaxStackSize()));
					} else {
						if (isUpgradeSlot(i)) {
							BackpackUpgradeSlot upgradeSlot = (BackpackUpgradeSlot) getSlot(i);
							IBackpackUpgradeItem<?> backpackUpgradeItem = (IBackpackUpgradeItem<?>) sourceStack.getItem();
							int newColumnsTaken = backpackUpgradeItem.getInventoryColumnsTaken();
							if (!needsSlotsThatAreOccupied(sourceStack, 0, upgradeSlot, newColumnsTaken)) {
								destSlot.set(sourceStack.split(toTransfer));
								updateColumnsTaken(newColumnsTaken);
							}
						} else {
							destSlot.set(sourceStack.split(toTransfer));
						}
					}

					destSlot.setChanged();
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
	public void setSynchronizer(ContainerSynchronizer synchronizer) {
		if (synchronizer instanceof ServerPlayer serverPlayer && backpackWrapper.getInventoryHandler().getStackSizeMultiplier() > 1) {
			super.setSynchronizer(new HighStackCountSynchronizer(serverPlayer));
			return;
		}
		super.setSynchronizer(synchronizer);
	}

	@Override
	public void removed(Player player) {
		for (Slot slot : upgradeSlots) {
			if (!(slot instanceof BackpackUpgradeSlot) && isInventorySlotInUpgradeTab(player, slot) && slot.getItem().getItem() instanceof BackpackItem &&
					!backpackWrapper.getInventoryHandler().isItemValid(0, slot.getItem())) {
				ItemStack slotStack = slot.getItem();
				slot.set(ItemStack.EMPTY);
				if (!player.addItem(slotStack)) {
					player.drop(slotStack, false);
				}
			}
		}
		super.removed(player);
		if (!player.level.isClientSide) {
			removeOpenTabIfKeepOff();
		}
	}

	private void removeOpenTabIfKeepOff() {
		if (Boolean.FALSE.equals(BackpackSettingsManager.getBackpackSettingValue(player, backpackWrapper.getSettingsHandler().getTypeCategory(BackpackSettingsCategory.class), BackpackSettingsManager.KEEP_TAB_OPEN))) {
			backpackWrapper.removeOpenTabId();
		}
	}

	private boolean isInventorySlotInUpgradeTab(Player player, Slot slot) {
		return slot.mayPickup(player) && !(slot instanceof ResultSlot);
	}

	public void setSlotStackToUpdate(int slot, ItemStack stack) {
		slotStacksToUpdate.put(slot, stack);
	}

	@Override
	public void setRemoteSlot(int slotIndex, ItemStack stack) {
		if (slotIndex < getInventorySlotsSize()) {
			remoteRealSlots.set(slotIndex, stack.copy());
		} else {
			remoteUpgradeSlots.set(slotIndex, stack.copy());
		}
	}

	@Override
	public void setRemoteSlotNoCopy(int slotIndex, ItemStack stack) {
		if (slotIndex < getInventorySlotsSize()) {
			remoteRealSlots.set(slotIndex, stack);
		} else {
			remoteUpgradeSlots.set(slotIndex, stack);
		}
	}

	@Override
	public OptionalInt findSlot(Container container, int slotIdx) {
		for (int i = 0; i < getTotalSlotsNumber(); ++i) {
			Slot slot = getSlot(i);
			if (slot.container == container && slotIdx == slot.getContainerSlot()) {
				return OptionalInt.of(i);
			}
		}
		return OptionalInt.empty();
	}

	@Override
	public void sendAllDataToRemote() {
		for (int i = 0; i < getInventorySlotsSize(); ++i) {
			remoteRealSlots.set(i, realInventorySlots.get(i).getItem().copy());
		}

		for (int i = 0; i < upgradeSlots.size(); ++i) {
			remoteUpgradeSlots.set(i, upgradeSlots.get(i).getItem().copy());
		}

		NonNullList<ItemStack> allRemoteSlots = NonNullList.create();
		allRemoteSlots.addAll(remoteRealSlots);
		allRemoteSlots.addAll(remoteUpgradeSlots);

		remoteCarried = getCarried().copy();

		if (synchronizer != null) {
			synchronizer.sendInitialData(this, allRemoteSlots, remoteCarried, new int[] {});
		}
	}
}
