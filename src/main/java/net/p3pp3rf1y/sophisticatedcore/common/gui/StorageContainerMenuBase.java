package net.p3pp3rf1y.sophisticatedcore.common.gui;

import com.google.common.base.Suppliers;
import com.google.common.collect.Lists;
import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.ints.IntComparators;
import net.minecraft.core.BlockPos;
import net.minecraft.core.NonNullList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.protocol.game.ClientboundContainerSetSlotPacket;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ClickAction;
import net.minecraft.world.inventory.ClickType;
import net.minecraft.world.inventory.ContainerListener;
import net.minecraft.world.inventory.ContainerSynchronizer;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.ResultSlot;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.network.SyncContainerClientDataMessage;
import net.p3pp3rf1y.sophisticatedcore.settings.ISlotColorCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsManager;
import net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable.GlobalOverridableSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IOverflowResponseUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.util.NoopStorageWrapper;

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

public abstract class StorageContainerMenuBase<S extends IStorageWrapper> extends AbstractContainerMenu {
	public static final int NUMBER_OF_PLAYER_SLOTS = 36;
	public static final ResourceLocation EMPTY_UPGRADE_SLOT_BACKGROUND = new ResourceLocation(SophisticatedCore.MOD_ID, "item/empty_upgrade_slot");
	protected static final String UPGRADE_ENABLED_TAG = "upgradeEnabled";
	protected static final String UPGRADE_SLOT_TAG = "upgradeSlot";
	protected static final String ACTION_TAG = "action";
	protected static final String OPEN_TAB_ID_TAG = "openTabId";
	protected static final String SORT_BY_TAG = "sortBy";
	private static final Method ON_SWAP_CRAFT = ObfuscationReflectionHelper.findMethod(Slot.class, "m_6405_", int.class);
	public final NonNullList<ItemStack> lastUpgradeSlots = NonNullList.create();
	public final List<Slot> upgradeSlots = Lists.newArrayList();
	public final NonNullList<ItemStack> remoteUpgradeSlots = NonNullList.create();
	public final NonNullList<ItemStack> lastRealSlots = NonNullList.create();
	public final List<Slot> realInventorySlots = Lists.newArrayList();
	private final Map<Integer, UpgradeContainerBase<?, ?>> upgradeContainers = new LinkedHashMap<>();
	private final NonNullList<ItemStack> remoteRealSlots = NonNullList.create();
	protected final Player player;
	protected final S storageWrapper;
	protected final IStorageWrapper parentStorageWrapper;
	private final StorageBackgroundProperties storageBackgroundProperties;
	private final Map<Integer, ItemStack> slotStacksToUpdate = new HashMap<>();
	private final int storageItemSlotIndex;
	private final boolean shouldLockStorageItemSlot;
	private int storageItemSlotNumber = -1;
	private Consumer<StorageContainerMenuBase<?>> upgradeChangeListener = null;
	private boolean isUpdatingFromPacket = false;
	private long errorResultExpirationTime = 0;
	@Nullable
	private UpgradeSlotChangeResult errorUpgradeSlotChangeResult;
	private CompoundTag lastSettingsNbt = null;

	protected StorageContainerMenuBase(MenuType<?> pMenuType, int pContainerId, Player player, S storageWrapper, IStorageWrapper parentStorageWrapper, int storageItemSlotIndex, boolean shouldLockStorageItemSlot) {
		super(pMenuType, pContainerId);
		this.player = player;
		this.storageWrapper = storageWrapper;
		this.parentStorageWrapper = parentStorageWrapper;
		this.storageItemSlotIndex = storageItemSlotIndex;
		this.shouldLockStorageItemSlot = shouldLockStorageItemSlot;

		removeOpenTabIfKeepOff();
		storageWrapper.fillWithLoot(player);
		storageBackgroundProperties = (getNumberOfStorageInventorySlots() + storageWrapper.getColumnsTaken() * storageWrapper.getNumberOfSlotRows()) <= 81 ? StorageBackgroundProperties.REGULAR : StorageBackgroundProperties.WIDE;
		initSlotsAndContainers(player, storageItemSlotIndex, shouldLockStorageItemSlot);
	}

	public abstract Optional<BlockPos> getBlockPosition();

	protected void initSlotsAndContainers(Player player, int storageItemSlotIndex, boolean shouldLockStorageItemSlot) {
		int yPosition = addStorageInventorySlots();
		addPlayerInventorySlots(player.getInventory(), yPosition, storageItemSlotIndex, shouldLockStorageItemSlot);
		addUpgradeSlots(yPosition);
		addUpgradeSettingsContainers(player);
	}

	protected void addUpgradeSettingsContainers(Player player) {
		UpgradeHandler upgradeHandler = storageWrapper.getUpgradeHandler();
		upgradeHandler.getSlotWrappers().forEach((slot, wrapper) -> UpgradeContainerRegistry.instantiateContainer(player, slot, wrapper)
				.ifPresent(container -> upgradeContainers.put(slot, container)));

		for (UpgradeContainerBase<?, ?> container : upgradeContainers.values()) {
			container.getSlots().forEach(this::addUpgradeSlot);
			container.onInit();
		}

		storageWrapper.getOpenTabId().ifPresent(id -> {
			if (upgradeContainers.containsKey(id)) {
				upgradeContainers.get(id).setIsOpen(true);
			}
		});
	}

	private void addUpgradeSlots(int lastInventoryRowY) {
		UpgradeHandler upgradeHandler = storageWrapper.getUpgradeHandler();

		int numberOfSlots = upgradeHandler.getSlots();

		if (numberOfSlots == 0) {
			return;
		}

		int slotIndex = 0;

		int yPosition = lastInventoryRowY - 22 * numberOfSlots;

		while (slotIndex < upgradeHandler.getSlots()) {
			addUpgradeSlot(instantiateUpgradeSlot(upgradeHandler, slotIndex, yPosition));

			slotIndex++;
			yPosition += 22;
		}
	}

	public int getColumnsTaken() {
		return storageWrapper.getColumnsTaken();
	}

	public Optional<UpgradeSlotChangeResult> getErrorUpgradeSlotChangeResult() {
		if (errorUpgradeSlotChangeResult != null && player.level.getGameTime() >= errorResultExpirationTime) {
			errorResultExpirationTime = 0;
			errorUpgradeSlotChangeResult = null;
		}
		return Optional.ofNullable(errorUpgradeSlotChangeResult);
	}

	protected void sendStorageSettingsToClient() {
		//noop by default
	}

	protected abstract StorageUpgradeSlot instantiateUpgradeSlot(UpgradeHandler upgradeHandler, int slotIndex, int yPosition);

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

	public int getInventorySlotsSize() {
		return realInventorySlots.size();
	}

	public int getNumberOfStorageInventorySlots() {
		return storageWrapper.getInventoryHandler().getSlots();
	}

	public int getNumberOfUpgradeSlots() {
		return storageWrapper.getUpgradeHandler().getSlots();
	}

	public StorageBackgroundProperties getStorageBackgroundProperties() {
		return storageBackgroundProperties;
	}

	public Map<Integer, UpgradeContainerBase<?, ?>> getUpgradeContainers() {
		return upgradeContainers;
	}

	protected int addStorageInventorySlots() {
		InventoryHandler inventoryHandler = storageWrapper.getInventoryHandler();
		int slotIndex = 0;
		int yPosition = 18;

		Set<Integer> noSortSlotIndexes = getNoSortSlotIndexes();
		while (slotIndex < inventoryHandler.getSlots()) {
			int lineIndex = slotIndex % getSlotsOnLine();
			int finalSlotIndex = slotIndex;
			StorageInventorySlot slot = new StorageInventorySlot(player.level.isClientSide, storageWrapper, inventoryHandler, finalSlotIndex, lineIndex, yPosition);
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

	protected void addPlayerInventorySlots(Inventory playerInventory, int yPosition, int storageItemSlotIndex, boolean shouldLockStorageItemSlot) {
		int playerInventoryXOffset = storageBackgroundProperties.getPlayerInventoryXOffset();

		yPosition += 14;

		for (int i = 0; i < 3; ++i) {
			for (int j = 0; j < 9; ++j) {
				int slotIndex = j + i * 9 + 9;
				int xPosition = playerInventoryXOffset + 8 + j * 18;
				Slot slot = addStorageItemSafeSlot(playerInventory, yPosition, slotIndex, xPosition, storageItemSlotIndex, shouldLockStorageItemSlot);
				addSlotAndUpdateStorageItemSlotNumber(storageItemSlotIndex, shouldLockStorageItemSlot, slotIndex, slot);
			}
			yPosition += 18;
		}

		yPosition += 4;

		for (int slotIndex = 0; slotIndex < 9; ++slotIndex) {
			int xPosition = playerInventoryXOffset + 8 + slotIndex * 18;
			Slot slot = addStorageItemSafeSlot(playerInventory, yPosition, slotIndex, xPosition, storageItemSlotIndex, shouldLockStorageItemSlot);
			addSlotAndUpdateStorageItemSlotNumber(storageItemSlotIndex, shouldLockStorageItemSlot, slotIndex, slot);
		}
	}

	private Slot addStorageItemSafeSlot(Inventory playerInventory, int yPosition, int slotIndex, int xPosition, int storageItemSlotIndex, boolean shouldLockStorageItemSlot) {
		Slot slot;
		if (shouldLockStorageItemSlot && slotIndex == storageItemSlotIndex) {
			slot = new Slot(playerInventory, slotIndex, xPosition, yPosition) {
				@Override
				public boolean mayPickup(Player playerIn) {
					return false;
				}

				@Override
				public void setChanged() {
					super.setChanged();
					closeScreenIfSomethingMessedWithStorageItemStack(getItem());
				}
			};
		} else {
			slot = new Slot(playerInventory, slotIndex, xPosition, yPosition);
		}

		return addSlot(slot);
	}

	public void closeScreenIfSomethingMessedWithStorageItemStack(ItemStack supposedToBeStorageItemStack) {
		if (!isClientSide() && isNotCorrectStorageItem(supposedToBeStorageItemStack)) {
			player.closeContainer();
		}
	}

	protected boolean isClientSide() {
		return player.level.isClientSide;
	}

	private void addSlotAndUpdateStorageItemSlotNumber(int storageItemSlotIndex, boolean lockStorageItemSlot, int slotIndex, Slot slot) {
		if (lockStorageItemSlot && slotIndex == storageItemSlotIndex) {
			storageItemSlotNumber = slot.index;
		}
	}

	public int getNumberOfRows() {
		return storageWrapper.getNumberOfSlotRows();
	}

	public int getSlotsOnLine() {
		return storageBackgroundProperties.getSlotsOnLine() - storageWrapper.getColumnsTaken();
	}

	public int getFirstUpgradeSlot() {
		return getInventorySlotsSize();
	}

	public boolean isFirstLevelStorage() {
		return parentStorageWrapper == NoopStorageWrapper.INSTANCE;
	}

	@Override
	public void initializeContents(int stateId, List<ItemStack> items, ItemStack carried) {
		storageWrapper.setPersistent(player.level.isClientSide);
		isUpdatingFromPacket = true;
		super.initializeContents(stateId, items, carried);
		isUpdatingFromPacket = false;
		storageWrapper.setPersistent(true);
		storageWrapper.getInventoryHandler().saveInventory();
		storageWrapper.getUpgradeHandler().saveInventory();
	}

	protected boolean isUpgradeSettingsSlot(int index) {
		return index >= getNumberOfStorageInventorySlots() + getNumberOfUpgradeSlots() + StorageContainerMenuBase.NUMBER_OF_PLAYER_SLOTS;
	}

	public boolean isStorageInventorySlot(int index) {
		return index >= 0 && index < getNumberOfStorageInventorySlots();
	}

	protected boolean isUpgradeSlot(int index) {
		return index >= getFirstUpgradeSlot() && (index - getFirstUpgradeSlot() < getNumberOfUpgradeSlots());
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
		} else if (isUpgradeSlot(slotId) && getSlot(slotId) instanceof StorageContainerMenuBase<?>.StorageUpgradeSlot slot) {
			ItemStack slotStack = slot.getItem();
			if (slot.mayPlace(getCarried())) {
				ItemStack cursorStack = getCarried();
				IUpgradeItem<?> upgradeItem = (IUpgradeItem<?>) cursorStack.getItem();
				int newColumnsTaken = upgradeItem.getInventoryColumnsTaken();
				int currentColumnsTaken = 0;
				if (!slotStack.isEmpty()) {
					currentColumnsTaken = ((IUpgradeItem<?>) slotStack.getItem()).getInventoryColumnsTaken();
				}
				if (needsSlotsThatAreOccupied(cursorStack, currentColumnsTaken, slot, newColumnsTaken)) {
					return;
				}

				int columnsToRemove = newColumnsTaken - currentColumnsTaken;
				if (slotStack.isEmpty() || slot.canSwapStack(player, cursorStack)) {
					setCarried(slotStack);
					slot.set(cursorStack);
					updateColumnsTaken(columnsToRemove);
					slot.setChanged();
				}
			} else if ((getCarried().isEmpty() || slot.mayPlace(getCarried())) && !slotStack.isEmpty() && slot.mayPickup(player)) {
				int k2 = dragType == 0 ? Math.min(slotStack.getCount(), slotStack.getMaxStackSize()) : Math.min(slotStack.getMaxStackSize() + 1, slotStack.getCount() + 1) / 2;
				int columnsTaken = ((IUpgradeItem<?>) slotStack.getItem()).getInventoryColumnsTaken();
				if (clickType == ClickType.QUICK_MOVE) {
					quickMoveStack(player, slotId);
				} else {
					setCarried(slot.remove(k2));
				}
				updateColumnsTaken(-columnsTaken);
				slot.onTake(player, getCarried());
			}
			return;
		} else if (isOverflowLogicSlotAndAction(slotId, clickType) && handleOverflow(slotId, clickType, dragType, player)) {
			return;
		}

		super.clicked(slotId, dragType, clickType, player);
	}

	@Override
	public boolean isValidSlotIndex(int slotIndex) {
		return slotIndex == -1 || slotIndex == -999 || slotIndex < getTotalSlotsNumber();
	}

	private boolean handleOverflow(int slotId, ClickType clickType, int dragType, Player player) {
		ItemStack cursorStack = clickType == ClickType.SWAP ? player.getInventory().getItem(dragType) : getCarried();
		Consumer<ItemStack> updateCursorStack = clickType == ClickType.SWAP ? s -> player.getInventory().setItem(dragType, s) : this::setCarried;
		Slot slot = getSlot(slotId);
		if ((clickType != ClickType.SWAP && cursorStack.isEmpty()) || !slot.mayPlace(cursorStack)) {
			return false;
		}
		ItemStack slotStack = slot.getItem();
		if (slotStack.isEmpty() || (slot.mayPickup(player) && slotStack.getItem() != cursorStack.getItem() && cursorStack.getCount() <= slot.getMaxStackSize(cursorStack) && slotStack.getCount() <= slotStack.getMaxStackSize())) {
			return processOverflowIfSlotWithSameItemFound(slotId, cursorStack, updateCursorStack);
		} else if (slotStack.getItem() == cursorStack.getItem()) {
			return processOverflowForAnythingOverSlotMaxSize(cursorStack, updateCursorStack, slot, slotStack);
		}
		return false;
	}

	private boolean processOverflowForAnythingOverSlotMaxSize(ItemStack cursorStack, Consumer<ItemStack> updateCursorStack, Slot slot, ItemStack slotStack) {
		int remainingSpaceInSlot = slot.getMaxStackSize(cursorStack) - slotStack.getCount();
		if (remainingSpaceInSlot < cursorStack.getCount()) {
			ItemStack overflow = cursorStack.copy();
			int overflowCount = cursorStack.getCount() - remainingSpaceInSlot;
			overflow.setCount(overflowCount);
			ItemStack result = processOverflowLogic(overflow);
			if (result.getCount() < overflowCount) {
				cursorStack.shrink(overflowCount - result.getCount());
				if (cursorStack.isEmpty()) {
					updateCursorStack.accept(ItemStack.EMPTY);
					return true;
				} else {
					updateCursorStack.accept(cursorStack);
				}
			}
		}
		return false;
	}

	private boolean processOverflowIfSlotWithSameItemFound(int slotId, ItemStack cursorStack, Consumer<ItemStack> updateCursorStack) {
		for (IOverflowResponseUpgrade overflowUpgrade : storageWrapper.getUpgradeHandler().getWrappersThatImplement(IOverflowResponseUpgrade.class)) {
			if (overflowUpgrade.stackMatchesFilter(cursorStack) && overflowUpgrade.worksInGui() 
					&& findSlotWithMatchingStack(slotId, cursorStack, updateCursorStack, overflowUpgrade)) {
				return true;
			}
		}
		return false;
	}

	private boolean findSlotWithMatchingStack(int slotId, ItemStack cursorStack, Consumer<ItemStack> updateCursorStack, IOverflowResponseUpgrade overflowUpgrade) {
		for (int slotIndex = 0; slotIndex < getNumberOfStorageInventorySlots(); slotIndex++) {
			if (slotIndex != slotId && overflowUpgrade.stackMatchesFilterStack(getSlot(slotIndex).getItem(), cursorStack)) {
				ItemStack result = cursorStack;
				result = overflowUpgrade.onOverflow(result);
				updateCursorStack.accept(result);
				if (result.isEmpty()) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean isOverflowLogicSlotAndAction(int slotId, ClickType clickType) {
		return isStorageInventorySlot(slotId) && (clickType == ClickType.SWAP || clickType == ClickType.PICKUP);
	}

	protected void updateColumnsTaken(int columnsToRemove) {
		if (columnsToRemove != 0) {
			storageWrapper.setColumnsTaken(Math.max(0, storageWrapper.getColumnsTaken() + columnsToRemove));
			storageWrapper.onContentsNbtUpdated();
			refreshAllSlots();
		}
	}

	protected boolean needsSlotsThatAreOccupied(ItemStack cursorStack, int currentColumnsTaken, StorageContainerMenuBase<?>.StorageUpgradeSlot upgradeSlot, int newColumnsTaken) {
		if (currentColumnsTaken >= newColumnsTaken) {
			return false;
		}

		int slotsToCheck = (newColumnsTaken - currentColumnsTaken) * getNumberOfRows();

		InventoryHandler invHandler = storageWrapper.getInventoryHandler();
		Set<Integer> errorSlots = new HashSet<>();
		int slots = getNumberOfStorageInventorySlots();
		for (int slotIndex = slots - 1; slotIndex >= slots - slotsToCheck; slotIndex--) {
			if (!invHandler.getStackInSlot(slotIndex).isEmpty()) {
				errorSlots.add(slotIndex);
			}
		}

		if (!errorSlots.isEmpty()) {
			upgradeSlot.updateSlotChangeError(new UpgradeSlotChangeResult.Fail(TranslationHelper.INSTANCE.translError("add.needs_occupied_inventory_slots", slotsToCheck, cursorStack.getHoverName()), Collections.emptySet(), errorSlots, Collections.emptySet()));
			return true;
		}
		return false;
	}

	public int getUpgradeSlotsSize() {
		return upgradeSlots.size();
	}

	public List<Integer> getSlotOverlayColors(int slot) {
		List<Integer> ret = new ArrayList<>();
		storageWrapper.getSettingsHandler().getCategoriesThatImplement(ISlotColorCategory.class).forEach(c -> c.getSlotColor(slot).ifPresent(ret::add));
		return ret;
	}

	public Optional<UpgradeContainerBase<?, ?>> getOpenContainer() {
		return storageWrapper.getOpenTabId().flatMap(id -> upgradeContainers.containsKey(id) ? Optional.of(upgradeContainers.get(id)) : Optional.empty());
	}

	protected void sendToServer(Consumer<CompoundTag> addData) {
		CompoundTag data = new CompoundTag();
		addData.accept(data);
		SophisticatedCore.PACKET_HANDLER.sendToServer(new SyncContainerClientDataMessage(data));
	}

	public void setUpgradeEnabled(int upgradeSlot, boolean enabled) {
		Map<Integer, IUpgradeWrapper> slotWrappers = storageWrapper.getUpgradeHandler().getSlotWrappers();
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

	public boolean getUpgradeEnabled(int upgradeSlot) {
		Map<Integer, IUpgradeWrapper> slotWrappers = storageWrapper.getUpgradeHandler().getSlotWrappers();
		if (!slotWrappers.containsKey(upgradeSlot)) {
			return false;
		}
		return slotWrappers.get(upgradeSlot).isEnabled();
	}

	public boolean canDisableUpgrade(int upgradeSlot) {
		Map<Integer, IUpgradeWrapper> slotWrappers = storageWrapper.getUpgradeHandler().getSlotWrappers();
		if (!slotWrappers.containsKey(upgradeSlot)) {
			return false;
		}
		return slotWrappers.get(upgradeSlot).canBeDisabled();
	}

	public void sort() {
		if (isClientSide()) {
			sendToServer(data -> data.putString(ACTION_TAG, "sort"));
			return;
		}

		storageWrapper.sort();
	}

	public void setOpenTabId(int tabId) {
		if (isClientSide()) {
			sendToServer(data -> data.putInt(OPEN_TAB_ID_TAG, tabId));
		}

		if (tabId == -1) {
			storageWrapper.removeOpenTabId();
		} else {
			storageWrapper.setOpenTabId(tabId);
		}
	}

	public void removeOpenTabId() {
		setOpenTabId(-1);
	}

	public SortBy getSortBy() {
		return storageWrapper.getSortBy();
	}

	public void setSortBy(SortBy sortBy) {
		if (isClientSide()) {
			sendToServer(data -> data.putString(SORT_BY_TAG, sortBy.getSerializedName()));
		}
		storageWrapper.setSortBy(sortBy);
	}

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
					//noop
				}
			}
		} else if (data.contains(UPGRADE_ENABLED_TAG)) {
			setUpgradeEnabled(data.getInt(UPGRADE_SLOT_TAG), data.getBoolean(UPGRADE_ENABLED_TAG));
		}
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
	public ItemStack quickMoveStack(Player player, int index) {
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
			return mergeStackToStorage(slotStack) || mergeStackToPlayersInventory(slotStack);
		} else if (isStorageInventorySlot(index)) {
			if (shouldShiftClickIntoOpenTabFirst()) {
				return mergeStackToOpenUpgradeTab(slotStack) || mergeStackToPlayersInventory(slotStack);
			}
			return mergeStackToPlayersInventory(slotStack) || mergeStackToOpenUpgradeTab(slotStack);
		} else if (isUpgradeSettingsSlot(index)) {
			if (getSlotUpgradeContainer(slot).map(c -> c.mergeIntoStorageFirst(slot)).orElse(true)) {
				return mergeStackToStorage(slotStack) || mergeStackToPlayersInventory(slotStack);
			}
			return mergeStackToPlayersInventory(slotStack) || mergeStackToStorage(slotStack);
		} else {
			if (shouldShiftClickIntoOpenTabFirst()) {
				return mergeStackToOpenUpgradeTab(slotStack) || mergeStackToUpgradeSlots(slotStack) || mergeStackToStorage(slotStack);
			}
			return mergeStackToUpgradeSlots(slotStack) || mergeStackToStorage(slotStack) || mergeStackToOpenUpgradeTab(slotStack);
		}
	}

	private boolean shouldShiftClickIntoOpenTabFirst() {
		GlobalOverridableSettingsCategory category = storageWrapper.getSettingsHandler().getTypeCategory(GlobalOverridableSettingsCategory.class);
		return SettingsManager.getSettingValue(player, category.getPlayerSettingsTagName(), category, SettingsManager.SHIFT_CLICK_INTO_OPEN_TAB_FIRST);
	}

	private boolean mergeStackToUpgradeSlots(ItemStack slotStack) {
		return !upgradeSlots.isEmpty() && moveItemStackTo(slotStack, getInventorySlotsSize(), getInventorySlotsSize() + getNumberOfUpgradeSlots(), false);
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

	private boolean mergeStackToStorage(ItemStack slotStack) {
		return mergeItemStack(slotStack, 0, getNumberOfStorageInventorySlots(), false, false, true);
	}

	private boolean mergeStackToPlayersInventory(ItemStack slotStack) {
		return mergeItemStack(slotStack, getNumberOfStorageInventorySlots(), getInventorySlotsSize(), true, true);
	}

	public boolean isPlayersInventorySlot(int slotNumber) {
		return slotNumber >= getNumberOfStorageInventorySlots() && slotNumber < getInventorySlotsSize();
	}

	public Optional<ItemStack> getMemorizedStackInSlot(int slotId) {
		return storageWrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class).getSlotFilterItem(slotId).map(ItemStack::new);
	}

	public void setUpgradeChangeListener(Consumer<StorageContainerMenuBase<?>> upgradeChangeListener) {
		this.upgradeChangeListener = upgradeChangeListener;
	}

	public abstract void openSettings();

	protected abstract boolean isNotCorrectStorageItem(ItemStack supposedToBeStorageItemStack);

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

	public int getTotalSlotsNumber() {
		return getInventorySlotsSize() + upgradeSlots.size();
	}

	protected void removeOpenTabIfKeepOff() {
		GlobalOverridableSettingsCategory category = storageWrapper.getSettingsHandler().getTypeCategory(GlobalOverridableSettingsCategory.class);
		if (Boolean.FALSE.equals(SettingsManager.getSettingValue(player, category.getPlayerSettingsTagName(), category, SettingsManager.KEEP_TAB_OPEN))) {
			storageWrapper.removeOpenTabId();
		}
	}

	protected Set<Integer> getNoSortSlotIndexes() {
		SettingsHandler settingsHandler = storageWrapper.getSettingsHandler();
		Set<Integer> slotIndexesExcludedFromSort = new HashSet<>();
		slotIndexesExcludedFromSort.addAll(settingsHandler.getTypeCategory(NoSortSettingsCategory.class).getNoSortSlots());
		slotIndexesExcludedFromSort.addAll(settingsHandler.getTypeCategory(MemorySettingsCategory.class).getSlotIndexes());
		return slotIndexesExcludedFromSort;
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

	protected void triggerSlotListeners(int stackIndex, ItemStack slotStack, Supplier<ItemStack> slotStackCopy, NonNullList<ItemStack> lastSlotsCollection, int slotIndexOffset) {
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
			remoteUpgradeSlots.set(slotIndex - getInventorySlotsSize(), stack);
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

		initSlotsAndContainers(player, storageItemSlotIndex, shouldLockStorageItemSlot);
	}

	protected ItemStack processOverflowLogic(ItemStack stack) {
		ItemStack result = stack;
		for (IOverflowResponseUpgrade overflowUpgrade : storageWrapper.getUpgradeHandler().getWrappersThatImplement(IOverflowResponseUpgrade.class)) {
			if (overflowUpgrade.worksInGui()) {
				result = overflowUpgrade.onOverflow(result);
				if (result.isEmpty()) {
					break;
				}
			}
		}
		return result;
	}

	private void onSwapCraft(Slot slot, int numItemsCrafted) {
		try {
			ON_SWAP_CRAFT.invoke(slot, numItemsCrafted);
		}
		catch (IllegalAccessException | InvocationTargetException e) {
			SophisticatedCore.LOGGER.error("Error invoking onSwapCraft method in Slot class", e);
		}
	}

	//copy of Container's doClick with the replacement of inventorySlots.get to getSlot, call to onswapcraft as that's protected in vanilla and an addition of upgradeSlots to pickup all
	@SuppressWarnings("java:S3776")
	//complexity here is brutal, but it's something that's in vanilla and need to keep this as close to it as possible for easier ports
	@Override
	protected void doClick(int slotId, int dragType, ClickType clickType, Player player) {
		Inventory inventory = player.getInventory();
		if (clickType == ClickType.QUICK_CRAFT) {
			int i = quickcraftStatus;
			quickcraftStatus = getQuickcraftHeader(dragType);
			if ((i != 1 || quickcraftStatus != 2) && i != quickcraftStatus) {
				resetQuickCraft();
			} else if (getCarried().isEmpty()) {
				resetQuickCraft();
			} else if (quickcraftStatus == 0) {
				quickcraftType = getQuickcraftType(dragType);
				if (isValidQuickcraftType(quickcraftType, player)) {
					quickcraftStatus = 1;
					quickcraftSlots.clear();
				} else {
					resetQuickCraft();
				}
			} else if (quickcraftStatus == 1) {
				Slot slot = getSlot(slotId);
				ItemStack itemstack = getCarried();
				if (StorageContainerMenuBase.canItemQuickReplace(slot, itemstack) && slot.mayPlace(itemstack) && (quickcraftType == 2 || itemstack.getCount() > quickcraftSlots.size()) && canDragTo(slot)) {
					quickcraftSlots.add(slot);
				}
			} else if (quickcraftStatus == 2) {
				if (!quickcraftSlots.isEmpty()) {
					if (quickcraftSlots.size() == 1) {
						int l = (quickcraftSlots.iterator().next()).index;
						resetQuickCraft();
						clicked(l, quickcraftType, ClickType.PICKUP, player);
						return;
					}

					ItemStack carried = getCarried().copy();
					int j1 = getCarried().getCount();

					for (Slot slot1 : quickcraftSlots) {
						ItemStack itemstack1 = getCarried();
						if (slot1 != null && StorageContainerMenuBase.canItemQuickReplace(slot1, itemstack1) && slot1.mayPlace(itemstack1) && (quickcraftType == 2 || itemstack1.getCount() >= quickcraftSlots.size()) && canDragTo(slot1)) {
							ItemStack carriedCopy = carried.copy();
							int j = slot1.hasItem() ? slot1.getItem().getCount() : 0;
							getQuickCraftSlotCount(quickcraftSlots, quickcraftType, carriedCopy, j);
							int slotStackLimit = slot1.getMaxStackSize(carriedCopy);
							if (!(slot1 instanceof StorageInventorySlot) && slotStackLimit > carriedCopy.getMaxStackSize()) {
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
		} else if ((clickType == ClickType.PICKUP || clickType == ClickType.QUICK_MOVE) && (dragType == 0 || dragType == 1)) {
			ClickAction clickaction = dragType == 0 ? ClickAction.PRIMARY : ClickAction.SECONDARY;
			if (slotId == -999) {
				if (!getCarried().isEmpty()) {
					if (clickaction == ClickAction.PRIMARY) {
						player.drop(getCarried(), true);
						setCarried(ItemStack.EMPTY);
					} else {
						player.drop(getCarried().split(1), true);
					}
				}
			} else if (clickType == ClickType.QUICK_MOVE) {
				if (slotId < 0) {
					return;
				}

				Slot slot6 = getSlot(slotId);
				if (!slot6.mayPickup(player)) {
					return;
				}

				if (isStorageInventorySlot(slotId)) {
					quickMoveStack(this.player, slotId).copy();
				} else {
					ItemStack itemstack8 = quickMoveStack(this.player, slotId);
					while (!itemstack8.isEmpty() && ItemStack.isSame(slot6.getItem(), itemstack8)) {
						itemstack8 = quickMoveStack(this.player, slotId);
					}
				}
			} else {
				if (slotId < 0) {
					return;
				}

				Slot slot7 = getSlot(slotId);
				ItemStack slotStack = slot7.getItem();
				ItemStack carriedStack = getCarried();
				player.updateTutorialInventoryAction(carriedStack, slot7.getItem(), clickaction);
				if (!carriedStack.overrideStackedOnOther(slot7, clickaction, player) && !slotStack.overrideOtherStackedOnMe(carriedStack, slot7, clickaction, player, createCarriedSlotAccess())) {
					if (slotStack.isEmpty()) {
						if (!carriedStack.isEmpty()) {
							int l2 = clickaction == ClickAction.PRIMARY ? carriedStack.getCount() : 1;
							setCarried(slot7.safeInsert(carriedStack, l2));
						}
					} else if (slot7.mayPickup(player)) {
						if (carriedStack.isEmpty()) {
							int i3 = clickaction == ClickAction.PRIMARY ? Math.min(slotStack.getCount(), slotStack.getMaxStackSize()) : Math.min(slotStack.getMaxStackSize() + 1, slotStack.getCount() + 1) / 2;
							Optional<ItemStack> optional1 = slot7.tryRemove(i3, Integer.MAX_VALUE, player);
							optional1.ifPresent((p_150421_) -> {
								setCarried(p_150421_);
								slot7.onTake(player, p_150421_);
							});
						} else if (slot7.mayPlace(carriedStack)) {
							if (ItemStack.isSameItemSameTags(slotStack, carriedStack)) {
								int j3 = clickaction == ClickAction.PRIMARY ? carriedStack.getCount() : 1;
								setCarried(slot7.safeInsert(carriedStack, j3));
							} else if (carriedStack.getCount() <= slot7.getMaxStackSize(carriedStack) && slotStack.getCount() <= slotStack.getMaxStackSize()) {
								slot7.set(carriedStack);
								setCarried(slotStack);
							}
						} else if (ItemStack.isSameItemSameTags(slotStack, carriedStack)) {
							Optional<ItemStack> optional = slot7.tryRemove(slotStack.getCount(), carriedStack.getMaxStackSize() - carriedStack.getCount(), player);
							optional.ifPresent((p_150428_) -> {
								carriedStack.grow(p_150428_.getCount());
								slot7.onTake(player, p_150428_);
							});
						}
					}
				}

				slot7.setChanged();
			}
		} else if (clickType == ClickType.SWAP) {
			Slot slot2 = getSlot(slotId);
			ItemStack itemstack4 = inventory.getItem(dragType);
			ItemStack slotStack = slot2.getItem();
			if (!itemstack4.isEmpty() || !slotStack.isEmpty()) {
				if (itemstack4.isEmpty()) {
					if (slot2.mayPickup(player)) {
						if (slotStack.getCount() <= slotStack.getMaxStackSize()) {
							inventory.setItem(dragType, slotStack);
							onSwapCraft(slot2, slotStack.getCount());
							slot2.set(ItemStack.EMPTY);
							slot2.onTake(player, slotStack);
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
				} else if (slotStack.getCount() <= slotStack.getMaxStackSize() && slot2.mayPickup(player) && slot2.mayPlace(itemstack4)) {
					int i2 = slot2.getMaxStackSize(itemstack4);
					if (itemstack4.getCount() > i2) {
						slot2.set(itemstack4.split(i2));
						slot2.onTake(player, slotStack);
						if (!inventory.add(slotStack)) {
							player.drop(slotStack, true);
						}
					} else {
						slot2.set(itemstack4);
						inventory.setItem(dragType, slotStack);
						slot2.onTake(player, slotStack);
					}
				}
			}
		} else if (clickType == ClickType.CLONE && player.getAbilities().instabuild && getCarried().isEmpty() && slotId >= 0) {
			Slot slot5 = getSlot(slotId);
			if (slot5.hasItem()) {
				ItemStack itemstack6 = slot5.getItem().copy();
				itemstack6.setCount(itemstack6.getMaxStackSize());
				setCarried(itemstack6);
			}
		} else if (clickType == ClickType.THROW && getCarried().isEmpty() && slotId >= 0) {
			Slot slot4 = getSlot(slotId);
			int i1 = dragType == 0 ? 1 : slot4.getItem().getCount();
			ItemStack itemstack8 = slot4.safeTake(i1, slot4.getItem().getMaxStackSize(), player);
			player.drop(itemstack8, true);
		} else if (clickType == ClickType.PICKUP_ALL && slotId >= 0) {
			Slot slot3 = getSlot(slotId);
			ItemStack carriedStack = getCarried();
			if (!carriedStack.isEmpty() && (!slot3.hasItem() || !slot3.mayPickup(player))) {
				int k1 = dragType == 0 ? 0 : getInventorySlotsSize() - 1;
				int j2 = dragType == 0 ? 1 : -1;

				for (int k2 = 0; k2 < 2; ++k2) {
					for (int k3 = k1; k3 >= 0 && k3 < getInventorySlotsSize() && carriedStack.getCount() < carriedStack.getMaxStackSize(); k3 += j2) {
						Slot slot8 = getSlot(k3);
						if (slot8.hasItem() && StorageContainerMenuBase.canItemQuickReplace(slot8, carriedStack) && slot8.mayPickup(player) && canTakeItemForPickAll(carriedStack, slot8)) {
							ItemStack itemstack12 = slot8.getItem();
							if (k2 != 0 || itemstack12.getCount() != itemstack12.getMaxStackSize()) {
								ItemStack itemstack13 = slot8.safeTake(itemstack12.getCount(), carriedStack.getMaxStackSize() - carriedStack.getCount(), player);
								carriedStack.grow(itemstack13.getCount());
							}
						}
					}
				}

				k1 = dragType == 0 ? 0 : upgradeSlots.size() - 1;

				for (int j = 0; j < 2; ++j) {
					for (int upgradeSlotId = k1; upgradeSlotId >= 0 && upgradeSlotId < upgradeSlots.size() && carriedStack.getCount() < carriedStack.getMaxStackSize(); upgradeSlotId += j2) {
						Slot upgradeSlot = upgradeSlots.get(upgradeSlotId);
						if (upgradeSlot.hasItem() && StorageContainerMenuBase.canItemQuickReplace(upgradeSlot, carriedStack) && upgradeSlot.mayPickup(this.player) && canTakeItemForPickAll(carriedStack, upgradeSlot)) {
							ItemStack itemstack3 = upgradeSlot.getItem();
							if (j != 0 || itemstack3.getCount() != itemstack3.getMaxStackSize()) {
								int l = Math.min(carriedStack.getMaxStackSize() - carriedStack.getCount(), itemstack3.getCount());
								ItemStack itemstack4 = upgradeSlot.remove(l);
								carriedStack.grow(l);
								if (itemstack4.isEmpty()) {
									upgradeSlot.set(ItemStack.EMPTY);
								}

								upgradeSlot.onTake(this.player, itemstack4);
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

	@Override
	public void removed(Player player) {
		for (Slot slot : upgradeSlots) {
			if (!(slot instanceof StorageContainerMenuBase<?>.StorageUpgradeSlot) && isInventorySlotInUpgradeTab(player, slot) && shouldSlotItemBeDroppedFromStorage(slot)) {
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

	protected static int calculateMaxCountForStack(int slotLimit, ItemStack stack) {
		return slotLimit / 64 * stack.getMaxStackSize();
	}

	//copy of mergeItemStack from Container - just calling getSlot here to account for upgrade slots instead of direct inventorySlots.get
	// and minor addition to be able to ignore magetslotx stack size
	@SuppressWarnings({"java:S3776", "java:S135"})
	//need to keep this very close to vanilla for easy port so not refactoring it to lower complexity or less exit points in loops
	protected boolean mergeItemStack(ItemStack sourceStack, int startIndex, int endIndex, boolean reverseDirection, boolean transferMaxStackSizeFromSource, boolean runOverflowLogic) {
		boolean mergedSomething = false;
		int i = startIndex;
		if (reverseDirection) {
			i = endIndex - 1;
		}

		int toTransfer = transferMaxStackSizeFromSource ? Math.min(sourceStack.getMaxStackSize(), sourceStack.getCount()) : sourceStack.getCount();
		if (runOverflowLogic || sourceStack.isStackable() || getSlot(startIndex).getMaxStackSize() > 64) {
			while (toTransfer > 0) {
				if (reverseDirection) {
					if (i < startIndex) {
						break;
					}
				} else if (i >= endIndex) {
					break;
				}

				Slot slot = getSlot(i);
				if (slot.mayPlace(sourceStack)) { //Added to vanilla logic as some slots may not want anything to be added to them
					ItemStack destStack = slot.getItem();
					if (!destStack.isEmpty() && ItemStack.isSameItemSameTags(sourceStack, destStack)) {
						int j = destStack.getCount() + toTransfer;
						int maxSize = StorageContainerMenuBase.calculateMaxCountForStack(slot.getMaxStackSize(), sourceStack);
						if (j <= maxSize) {
							sourceStack.shrink(toTransfer);
							destStack.setCount(j);
							toTransfer = 0;
							slot.setChanged();
							mergedSomething = true;
						} else if (destStack.getCount() < maxSize) {
							sourceStack.shrink(maxSize - destStack.getCount());
							toTransfer -= maxSize - destStack.getCount();
							destStack.setCount(maxSize);
							slot.setChanged();
							mergedSomething = true;
						}

						if (runOverflowLogic && !sourceStack.isEmpty()) {
							ItemStack result = processOverflowLogic(sourceStack);
							if (result != sourceStack) {
								sourceStack.setCount(result.getCount());
								mergedSomething = true;
							}
						}
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
			int firstIndex = reverseDirection ? endIndex - 1 : startIndex;
			int increment = reverseDirection ? -1 : 1;

			MemorySettingsCategory memory = storageWrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class);
			for (int slotIndex = firstIndex; (reverseDirection ? slotIndex >= startIndex : slotIndex < endIndex) && toTransfer > 0; slotIndex += increment) {
				if (memory.getSlotIndexes().contains(slotIndex) && memory.matchesFilter(slotIndex, sourceStack)) {
					Slot slot = getSlot(slotIndex);
					if (!slot.mayPlace(sourceStack)) {
						continue;
					}
					ItemStack destStack = slot.getItem();
					if (destStack.isEmpty()) {
						slot.set(sourceStack.split(slot.getMaxStackSize()));
						slot.setChanged();
						toTransfer = sourceStack.getCount();
						mergedSomething = true;
					}
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
					boolean errorMerging = false;
					if (toTransfer > destSlot.getMaxStackSize()) {
						if (runOverflowLogic && processOverflowIfSlotWithSameItemFound(i, sourceStack, s -> {})) {
							sourceStack.shrink(sourceStack.getCount());
							mergedSomething = true;
						} else {
							destSlot.set(sourceStack.split(destSlot.getMaxStackSize()));
						}
					} else {
						if (isUpgradeSlot(i)) {
							StorageUpgradeSlot upgradeSlot = (StorageUpgradeSlot) getSlot(i);
							IUpgradeItem<?> upgradeItem = (IUpgradeItem<?>) sourceStack.getItem();
							int newColumnsTaken = upgradeItem.getInventoryColumnsTaken();
							if (!needsSlotsThatAreOccupied(sourceStack, 0, upgradeSlot, newColumnsTaken)) {
								destSlot.set(sourceStack.split(toTransfer));
								updateColumnsTaken(newColumnsTaken);
							} else {
								errorMerging = true;
							}
						} else {
							if (runOverflowLogic && processOverflowIfSlotWithSameItemFound(i, sourceStack, s -> {})) {
								sourceStack.shrink(sourceStack.getCount());
								mergedSomething = true;
							} else {
								destSlot.set(sourceStack.split(toTransfer));
							}
						}
					}
					if (!errorMerging) {
						destSlot.setChanged();
						mergedSomething = true;
						break;
					}
				}

				if (reverseDirection) {
					--i;
				} else {
					++i;
				}
			}
		}

		return mergedSomething;
	}

	@Override
	protected boolean moveItemStackTo(ItemStack stack, int startIndex, int endIndex, boolean reverseDirection) {
		return mergeItemStack(stack, startIndex, endIndex, reverseDirection, false);
	}

	protected boolean mergeItemStack(ItemStack sourceStack, int startIndex, int endIndex, boolean reverseDirection, boolean transferMaxStackSizeFromSource) {
		return mergeItemStack(sourceStack, startIndex, endIndex, reverseDirection, transferMaxStackSizeFromSource, false);
	}

	@Override
	public void setSynchronizer(ContainerSynchronizer synchronizer) {
		if (player instanceof ServerPlayer serverPlayer && storageWrapper.getInventoryHandler().getStackSizeMultiplier() > 1) {
			super.setSynchronizer(new HighStackCountSynchronizer(serverPlayer));
			return;
		}
		super.setSynchronizer(synchronizer);
	}

	public static boolean canItemQuickReplace(@Nullable Slot slot, ItemStack stack) {
		boolean flag = slot == null || !slot.hasItem();
		if (!flag && stack.sameItem(slot.getItem()) && ItemStack.tagMatches(slot.getItem(), stack)) {
			return slot.getItem().getCount() <= calculateMaxCountForStack(slot.getMaxStackSize(), stack);
		} else {
			return flag;
		}
	}

	@Override
	public Slot getSlot(int slotId) {
		if (slotId >= getInventorySlotsSize()) {
			return upgradeSlots.get(slotId - getInventorySlotsSize());
		} else {
			return realInventorySlots.get(slotId);
		}
	}

	@Override
	public void broadcastChanges() {
		if (storageItemSlotNumber != -1) {
			closeScreenIfSomethingMessedWithStorageItemStack(getSlot(storageItemSlotNumber).getItem());
		}

		synchronizeCarriedToRemote();
		broadcastChangesIn(lastUpgradeSlots, remoteUpgradeSlots, upgradeSlots, getFirstUpgradeSlot());
		broadcastChangesIn(lastRealSlots, remoteRealSlots, realInventorySlots, 0);

		if (lastSettingsNbt == null || !lastSettingsNbt.equals(storageWrapper.getSettingsHandler().getNbt())) {
			lastSettingsNbt = storageWrapper.getSettingsHandler().getNbt().copy();
			sendStorageSettingsToClient();
			refreshInventorySlotsIfNeeded();
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

	protected void refreshInventorySlotsIfNeeded() {
		Set<Integer> noSortSlotIndexes = getNoSortSlotIndexes();
		boolean needRefresh = false;
		if (getInventorySlotsSize() - slots.size() != noSortSlotIndexes.size()) {
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
		int yPosition = addStorageInventorySlots();
		addPlayerInventorySlots(player.getInventory(), yPosition, storageItemSlotIndex, shouldLockStorageItemSlot);
	}

	@Override
	public NonNullList<ItemStack> getItems() {
		NonNullList<ItemStack> list = NonNullList.create();

		realInventorySlots.forEach(slot -> list.add(slot.getItem()));
		upgradeSlots.forEach(upgradeSlot -> list.add(upgradeSlot.getItem()));
		return list;
	}

	public abstract void detectSettingsChangeAndReload();

	@SuppressWarnings("java:S1172") // slot parameter is used in overrides
	protected boolean shouldSlotItemBeDroppedFromStorage(Slot slot) {
		return false;
	}

	private boolean isInventorySlotInUpgradeTab(Player player, Slot slot) {
		return slot.mayPickup(player) && !(slot instanceof ResultSlot);
	}

	public void setSlotStackToUpdate(int slot, ItemStack stack) {
		slotStacksToUpdate.put(slot, stack);
	}

	public class StorageUpgradeSlot extends SlotItemHandler {
		private boolean wasEmpty = false;
		private final int slotIndex;

		public StorageUpgradeSlot(UpgradeHandler upgradeHandler, int slotIndex, int yPosition) {
			super(upgradeHandler, slotIndex, -15, yPosition);
			this.slotIndex = slotIndex;
		}

		@Override
		public void setChanged() {
			super.setChanged();
			if ((!isUpdatingFromPacket && wasEmpty != getItem().isEmpty()) || updateWrappersAndCheckForReloadNeeded()) {
				reloadUpgradeControl();
				if (!isFirstLevelStorage()) {
					parentStorageWrapper.getUpgradeHandler().refreshUpgradeWrappers();
				}
				onUpgradeChanged();
			}
			wasEmpty = getItem().isEmpty();
		}

		protected void onUpgradeChanged() {
			//noop by default
		}

		@Override
		public boolean mayPlace(ItemStack stack) {
			if (stack.isEmpty() || !getItemHandler().isItemValid(slotIndex, stack)) {
				return false;
			}
			UpgradeSlotChangeResult result = ((IUpgradeItem<?>) stack.getItem()).canAddUpgradeTo(storageWrapper, stack, isFirstLevelStorage());
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

			UpgradeSlotChangeResult result = ((IUpgradeItem<?>) getItem().getItem()).canRemoveUpgradeFrom(storageWrapper);
			updateSlotChangeError(result);
			return result.isSuccessful();
		}

		public boolean canSwapStack(Player player, ItemStack stackToPut) {
			boolean ret = super.mayPickup(player);
			if (!ret) {
				return false;
			}
			UpgradeSlotChangeResult result = ((IUpgradeItem<?>) getItem().getItem()).canSwapUpgradeFor(stackToPut, storageWrapper);
			updateSlotChangeError(result);
			return result.isSuccessful();
		}

		private boolean updateWrappersAndCheckForReloadNeeded() {
			int checkedContainersCount = 0;
			for (Map.Entry<Integer, IUpgradeWrapper> slotWrapper : storageWrapper.getUpgradeHandler().getSlotWrappers().entrySet()) {
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
			storageWrapper.removeOpenTabId();
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
				upgradeChangeListener.accept(StorageContainerMenuBase.this);
			}
		}

		@Nullable
		@Override
		public Pair<ResourceLocation, ResourceLocation> getNoItemIcon() {
			return new Pair<>(InventoryMenu.BLOCK_ATLAS, StorageContainerMenuBase.EMPTY_UPGRADE_SLOT_BACKGROUND);
		}
	}
}
