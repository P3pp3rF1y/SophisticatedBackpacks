package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import it.unimi.dsi.fastutil.ints.IntComparators;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackBackgroundProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.ServerBackpackDataMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackUpgradeHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NoopBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.BACKPACK_BLOCK_CONTAINER_TYPE;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.BACKPACK_ITEM_CONTAINER_TYPE;

public class BackpackContainer extends Container {
	private static final int NUMBER_OF_PLAYER_SLOTS = 36;
	private static final String OPEN_TAB_ID_TAG = "openTabId";

	private final IBackpackWrapper backpackWrapper;
	private final PlayerEntity player;
	private int backpackSlotNumber = -1;

	private final Map<Integer, UpgradeContainerBase<?, ?>> upgradeContainers = new LinkedHashMap<>();
	private Consumer<BackpackContainer> upgradeChangeListener = null;

	private final List<Slot> backpackInventorySlots = new ArrayList<>();
	private final List<Slot> playerSlots = new ArrayList<>();

	private final BackpackBackgroundProperties backpackBackgroundProperties;

	public BackpackContainer(int windowId, PlayerEntity player, String handlerName, int backpackSlot) {
		super(BACKPACK_ITEM_CONTAINER_TYPE.get(), windowId);
		this.player = player;

		Optional<PlayerInventoryHandler> h = PlayerInventoryProvider.getPlayerInventoryHandler(handlerName);
		if (!h.isPresent()) {
			backpackWrapper = NoopBackpackWrapper.INSTANCE;
			backpackBackgroundProperties = BackpackBackgroundProperties.REGULAR;
			return;
		}
		PlayerInventoryHandler handler = h.get();
		backpackWrapper = handler.getStackInSlot(player, backpackSlot).getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY).orElse(NoopBackpackWrapper.INSTANCE);

		backpackBackgroundProperties = getNumberOfSlots() <= 81 ? BackpackBackgroundProperties.REGULAR : BackpackBackgroundProperties.WIDE;

		int yPosition = addBackpackInventorySlots();
		addBackpackUpgradeSlots(yPosition);
		addPlayerInventorySlots(player.inventory, yPosition, backpackSlot, handler.isVisibleInGui());
		addUpgradeSettingsContainers(player);
	}

	public BackpackContainer(int windowId, PlayerEntity player, BlockPos pos) {
		super(BACKPACK_BLOCK_CONTAINER_TYPE.get(), windowId);
		this.player = player;
		Optional<BackpackTileEntity> backpackTile = WorldHelper.getTile(player.world, pos, BackpackTileEntity.class);
		backpackWrapper = backpackTile.map(te -> te.getBackpackWrapper().orElse(NoopBackpackWrapper.INSTANCE)).orElse(NoopBackpackWrapper.INSTANCE);

		backpackBackgroundProperties = getNumberOfSlots() <= 81 ? BackpackBackgroundProperties.REGULAR : BackpackBackgroundProperties.WIDE;

		int yPosition = addBackpackInventorySlots();
		addBackpackUpgradeSlots(yPosition);
		addPlayerInventorySlots(player.inventory, yPosition, -1, false);
		addUpgradeSettingsContainers(player);
	}

	private void addUpgradeSettingsContainers(PlayerEntity player) {
		BackpackUpgradeHandler upgradeHandler = backpackWrapper.getUpgradeHandler();
		upgradeHandler.getSlotWrappers().forEach((slot, wrapper) -> UpgradeContainerRegistry.instantiateContainer(player, slot, wrapper)
				.ifPresent(container -> upgradeContainers.put(slot, container)));

		for (UpgradeContainerBase<?, ?> container : upgradeContainers.values()) {
			container.getSlots().forEach(this::addSlot);
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

		int yPosition = lastInventoryRowY - (22 + 22 * (numberOfSlots - 1));

		while (slotIndex < upgradeHandler.getSlots()) {
			addSlot(new BackpackUpgradeSlot(upgradeHandler, slotIndex, yPosition));

			slotIndex++;
			yPosition += 22;
		}
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
			backpackInventorySlots.add(addSlot(new BackpackInventorySlot(inventoryHandler, finalSlotIndex, lineIndex, yPosition)));

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

	private void addPlayerInventorySlots(PlayerInventory playerInventory, int yPosition, int slotIndex, boolean lockBackpackSlot) {
		int playerInventoryXOffset = backpackBackgroundProperties.getPlayerInventoryXOffset();

		yPosition += 14;

		for (int i = 0; i < 3; ++i) {
			for (int j = 0; j < 9; ++j) {
				playerSlots.add(addSlot(new Slot(playerInventory, j + i * 9 + 9, playerInventoryXOffset + 8 + j * 18, yPosition)));
			}
			yPosition += 18;
		}

		yPosition += 4;

		for (int k = 0; k < 9; ++k) {
			Slot slot = addSlot(new Slot(playerInventory, k, playerInventoryXOffset + 8 + k * 18, yPosition));
			if (lockBackpackSlot && k == slotIndex) {
				backpackSlotNumber = slot.slotNumber;
			} else {
				playerSlots.add(slot);
			}
		}
	}

	public int getNumberOfRows() {
		BackpackInventoryHandler invHandler = backpackWrapper.getInventoryHandler();
		return (int) Math.ceil((double) invHandler.getSlots() / getSlotsOnLine());
	}

	private int getSlotsOnLine() {
		return backpackBackgroundProperties.getSlotsOnLine();
	}

	@Override
	public boolean canInteractWith(PlayerEntity playerIn) {
		return true;
	}

	public static BackpackContainer fromBufferItem(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, packetBuffer.readString(), packetBuffer.readInt());
	}

	public static BackpackContainer fromBufferBlock(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, BlockPos.fromLong(packetBuffer.readLong()));
	}

	@Override
	public ItemStack transferStackInSlot(PlayerEntity playerIn, int index) {
		ItemStack itemstack = ItemStack.EMPTY;
		Slot slot = inventorySlots.get(index);
		if (slot != null && slot.getHasStack()) {
			if (index == backpackSlotNumber) {
				return ItemStack.EMPTY;
			}

			Optional<UpgradeContainerBase<?, ?>> upgradeContainer = getSlotUpgradeContainer(slot);
			ItemStack slotStack = upgradeContainer.map(c -> c.getSlotStackToTransfer(slot)).orElse(slot.getStack());
			itemstack = slotStack.copy();

			if (!mergeSlotStack(index, slotStack)) {
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

	private boolean mergeSlotStack(int index, ItemStack slotStack) {
		if (isBackpackInventoryOrUpgradeSlot(index)) {
			return mergeStackToPlayersInventory(slotStack);
		} else if (isUpgradeSettingsSlot(index)) {
			return mergeStackToBackpack(slotStack) || mergeStackToPlayersInventory(slotStack);
		} else {
			return mergeStackToUpgradeSlots(slotStack) || mergeStackToBackpack(slotStack);
		}
	}

	private boolean mergeStackToUpgradeSlots(ItemStack slotStack) {
		int backpackSlots = getBackpackSlotsCount();
		return mergeItemStack(slotStack, backpackSlots, backpackSlots + getNumberOfUpgradeSlots(), false);
	}

	private boolean mergeStackToBackpack(ItemStack slotStack) {
		return mergeItemStack(slotStack, 0, getBackpackSlotsCount(), false);
	}

	private boolean mergeStackToPlayersInventory(ItemStack slotStack) {
		return mergeItemStack(slotStack, getBackpackSlotsCount() + getNumberOfUpgradeSlots(), getFirstUpgradeSettingsSlot(), true);
	}

	private boolean isUpgradeSettingsSlot(int index) {
		return index >= getBackpackSlotsCount() + getNumberOfUpgradeSlots() + NUMBER_OF_PLAYER_SLOTS;
	}

	private boolean isBackpackInventoryOrUpgradeSlot(int index) {
		return index < getBackpackSlotsCount() + getNumberOfUpgradeSlots();
	}

	private Optional<UpgradeContainerBase<?, ?>> getSlotUpgradeContainer(Slot slot) {
		if (slot.slotNumber >= getFirstUpgradeSettingsSlot()) {
			for (UpgradeContainerBase<?, ?> upgradeContainer : upgradeContainers.values()) {
				if (upgradeContainer.containsSlot(slot)) {
					return Optional.of(upgradeContainer);
				}
			}
		}
		return Optional.empty();
	}

	private int getBackpackSlotsCount() {
		return getNumberOfRows() * getSlotsOnLine();
	}

	@Override
	public ItemStack slotClick(int slotId, int dragType, ClickType clickType, PlayerEntity player) {
		if (slotId == backpackSlotNumber) {
			return ItemStack.EMPTY;
		} else if (slotId >= getFirstUpgradeSettingsSlot() && getSlot(slotId) instanceof FilterSlotItemHandler && getSlot(slotId).isItemValid(player.inventory.getItemStack())) {
			ItemStack currentStack = player.inventory.getItemStack().copy();
			if (currentStack.getCount() > 1) {
				currentStack.setCount(1);
			}

			getSlot(slotId).putStack(currentStack);
			return ItemStack.EMPTY;
		}

		return super.slotClick(slotId, dragType, clickType, player);
	}

	@Override
	public boolean canMergeSlot(ItemStack stack, Slot slotIn) {
		return slotIn.slotNumber < getFirstUpgradeSettingsSlot();
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

	public Collection<UpgradeContainerBase<?, ?>> getUpgradeContainers() {
		return upgradeContainers.values();
	}

	public void handleMessage(CompoundNBT data) {
		if (data.contains("containerId")) {
			int containerId = data.getInt("containerId");
			if (upgradeContainers.containsKey(containerId)) {
				upgradeContainers.get(containerId).handleMessage(data);
			}
		} else if (data.contains(OPEN_TAB_ID_TAG)) {
			setOpenTabId(data.getInt(OPEN_TAB_ID_TAG));
		}
	}

	public List<Slot> getBackpackInventorySlots() {
		return backpackInventorySlots;
	}

	public Collection<? extends Slot> getPlayerInventorySlots() {
		return playerSlots;
	}

	public void setOpenTabId(int tabId) {
		if (player.world.isRemote) {
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

	public class BackpackUpgradeSlot extends SlotItemHandler {
		public BackpackUpgradeSlot(BackpackUpgradeHandler upgradeHandler, int slotIndex, int yPosition) {
			super(upgradeHandler, slotIndex, -18, yPosition);
		}

		@Override
		public void onSlotChanged() {
			super.onSlotChanged();
			if (updateWrappersAndCheckForReloadNeeded()) {
				reloadUpgradeControl();
			}
		}

		private boolean updateWrappersAndCheckForReloadNeeded() {
			int checkedContainersCount = 0;
			for (Map.Entry<Integer, IUpgradeWrapper> slotWrapper : backpackWrapper.getUpgradeHandler().getSlotWrappers().entrySet()) {
				UpgradeContainerBase<?, ?> container = upgradeContainers.get(slotWrapper.getKey());
				if (!slotWrapper.getValue().displaysSettingsTab()) {
					if (container != null) {
						return true;
					}
				} else if (container == null || container.getUpgradeWrapper() != slotWrapper.getValue()) {
					if (container == null || container.getUpgradeWrapper().getUpgradeStack().getItem() != slotWrapper.getValue().getUpgradeStack().getItem()) {
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
					slotNumbersToRemove.add(slot.slotNumber);
					inventorySlots.remove(slot);
				});
			}
			slotNumbersToRemove.sort(IntComparators.OPPOSITE_COMPARATOR);
			for (int slotNumber : slotNumbersToRemove) {
				inventoryItemStacks.remove(slotNumber);
			}
		}

		private void onUpgradesChanged() {
			if (upgradeChangeListener != null) {
				upgradeChangeListener.accept(BackpackContainer.this);
			}
		}
	}

	private class BackpackInventorySlot extends SlotItemHandler {
		public BackpackInventorySlot(BackpackInventoryHandler inventoryHandler, int finalSlotIndex, int lineIndex, int yPosition) {super(inventoryHandler, finalSlotIndex, 8 + lineIndex * 18, yPosition);}

		@Override
		public void onSlotChanged() {
			super.onSlotChanged();
			// saving here as well because there are many cases where vanilla modifies stack directly without and inventory handler isn't aware of it
			// however it does notify the slot of change
			backpackWrapper.getInventoryHandler().saveInventory();
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
}
