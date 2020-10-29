package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.items.SlotItemHandler;
import net.minecraftforge.registries.ObjectHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackInventoryEventBus;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackUpgradeHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import java.util.Optional;

public class BackpackContainer extends Container {
	@ObjectHolder(SophisticatedBackpacks.MOD_ID + ":backpack")
	public static ContainerType<BackpackContainer> TYPE;
	private final BackpackWrapper backPackWrapper;
	private int backpackSlotNumber = -1;

	public BackpackContainer(int windowId, PlayerEntity player, String handlerName, int backpackSlot) {
		super(TYPE, windowId);

		Optional<PlayerInventoryHandler> h = PlayerInventoryProvider.getPlayerInventoryHandler(handlerName);

		if (!h.isPresent()) {
			backPackWrapper = new BackpackWrapper(new ItemStack(ModItems.BACKPACK));
			return;
		}
		PlayerInventoryHandler handler = h.get();
		backPackWrapper = new BackpackWrapper(handler.getStackInSlot(player, backpackSlot));
		if (!player.world.isRemote) {
			backPackWrapper.setPersistent();
			BackpackInventoryEventBus.registerListener(player.getUniqueID(), (hName, backpackInSlot, slot, newStack) -> {
				if (hName.equals(handlerName) && backpackInSlot == backpackSlot) {
					backPackWrapper.onInventorySlotUpdate(slot, newStack);
				}
			});
		}
		int yPosition = addBackpackInventorySlots();
		addBackpackUpgradeSlots(yPosition);
		addPlayerInventorySlots(player.inventory, yPosition, backpackSlot, handler.isVisibleInGui());
	}

	private void addBackpackUpgradeSlots(int lastInventoryRowY) {
		BackpackUpgradeHandler upgradeHandler = backPackWrapper.getUpgradeHandler();

		int numberOfSlots = upgradeHandler.getSlots();

		if (numberOfSlots == 0) {
			return;
		}

		int slotIndex = 0;

		int yPosition = lastInventoryRowY - (22 + 22 * (numberOfSlots - 1));

		while (slotIndex < upgradeHandler.getSlots()) {
			addSlot(new SlotItemHandler(upgradeHandler, slotIndex, -18, yPosition));

			slotIndex++;
			yPosition += 22;
		}
	}

	private int addBackpackInventorySlots() {
		BackpackInventoryHandler inventoryHandler = backPackWrapper.getInventoryHandler();
		int slotIndex = 0;
		int yPosition = 18;

		while (slotIndex < inventoryHandler.getSlots()) {
			int lineIndex = slotIndex % getSlotsOnLine();
			int finalSlotIndex = slotIndex;
			addSlot(new SlotItemHandler(inventoryHandler, finalSlotIndex, 8 + lineIndex * 18, yPosition));

			slotIndex++;
			if (slotIndex % getSlotsOnLine() == 0) {
				yPosition += 18;
			}
		}

		return yPosition;
	}

	private void addPlayerInventorySlots(PlayerInventory playerInventory, int yPosition, int slotIndex, boolean lockBackpackSlot) {
		int playerInventoryYOffset = backPackWrapper.getScreenProperties().getPlayerInventoryYOffset();

		yPosition += 14;

		for (int i = 0; i < 3; ++i) {
			for (int j = 0; j < 9; ++j) {
				addSlot(new Slot(playerInventory, j + i * 9 + 9, playerInventoryYOffset + 8 + j * 18, yPosition));
			}
			yPosition += 18;
		}

		yPosition += 4;

		for (int k = 0; k < 9; ++k) {
			Slot slot = addSlot(new Slot(playerInventory, k, playerInventoryYOffset + 8 + k * 18, yPosition));
			if (lockBackpackSlot && k == slotIndex) {
				backpackSlotNumber = slot.slotNumber;
			}
		}
	}

	public int getNumberOfRows() {
		BackpackInventoryHandler invHandler = backPackWrapper.getInventoryHandler();
		return (int) Math.ceil((double) invHandler.getSlots() / getSlotsOnLine());
	}

	private int getSlotsOnLine() {
		return backPackWrapper.getScreenProperties().getSlotsOnLine();
	}

	@Override
	public boolean canInteractWith(PlayerEntity playerIn) {
		return true;
	}

	public static BackpackContainer fromBuffer(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new BackpackContainer(windowId, playerInventory.player, packetBuffer.readString(), packetBuffer.readInt());
	}

	@Override
	public ItemStack transferStackInSlot(PlayerEntity playerIn, int index) {
		ItemStack itemstack = ItemStack.EMPTY;
		Slot slot = inventorySlots.get(index);
		if (slot != null && slot.getHasStack()) {
			ItemStack slotStack = slot.getStack();

			if (index == backpackSlotNumber) {
				return ItemStack.EMPTY;
			}
			itemstack = slotStack.copy();
			int backpackSlots = getBackpackSlotsCount();
			if (index < backpackSlots + getNumberOfUpgradeSlots()) {
				if (!mergeItemStack(slotStack, backpackSlots + getNumberOfUpgradeSlots(), inventorySlots.size(), true)) {
					return ItemStack.EMPTY;
				}
			} else if (!mergeItemStack(slotStack, backpackSlots, backpackSlots + getNumberOfUpgradeSlots(), false)
					&& !mergeItemStack(slotStack, 0, backpackSlots, false)) {
				return ItemStack.EMPTY;
			}

			if (slotStack.isEmpty()) {
				slot.putStack(ItemStack.EMPTY);
			} else {
				slot.onSlotChanged();
			}
		}

		return itemstack;
	}

	private int getBackpackSlotsCount() {
		return getNumberOfRows() * getSlotsOnLine();
	}

	@Override
	public ItemStack slotClick(int slotId, int dragType, ClickType clickType, PlayerEntity player) {
		if (slotId == backpackSlotNumber) {
			return ItemStack.EMPTY;
		}
		boolean notifyOfChange = false;
		if (clickType == ClickType.PICKUP && slotId >= 0 && slotId < getBackpackSlotsCount() && getSlot(slotId).getHasStack() && !player.inventory.getItemStack().isEmpty()) {
			notifyOfChange = true; //fixing an issue where vanilla directly modifies stack and item handler isn't aware of that change in this case
		}
		ItemStack ret = super.slotClick(slotId, dragType, clickType, player);
		if (notifyOfChange) {
			backPackWrapper.getInventoryHandler().onContentsChanged(slotId);
		}
		return ret;
	}

	public int getNumberOfSlots() {
		return backPackWrapper.getInventoryHandler().getSlots();
	}

	public ScreenProperties getScreenProperties() {
		return backPackWrapper.getScreenProperties();
	}

	public int getNumberOfUpgradeSlots() {
		return backPackWrapper.getUpgradeHandler().getSlots();
	}

	@Override
	public void onContainerClosed(PlayerEntity playerIn) {
		super.onContainerClosed(playerIn);
		BackpackInventoryEventBus.unregisterListener(playerIn.getUniqueID());
	}
}
