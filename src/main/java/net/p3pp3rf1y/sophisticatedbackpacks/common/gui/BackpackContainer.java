package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.Hand;
import net.minecraftforge.items.SlotItemHandler;
import net.minecraftforge.registries.ObjectHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

public class BackpackContainer extends Container {
	@ObjectHolder(SophisticatedBackpacks.MOD_ID + ":backpack")
	public static ContainerType<BackpackContainer> TYPE;
	private final BackpackWrapper backPackWrapper;
	private int backpackSlotNumber = -1;

	public BackpackContainer(int windowId, PlayerInventory playerInventory, ItemStack backpack) {
		super(TYPE, windowId);

		backPackWrapper = new BackpackWrapper(backpack);
		int yPosition = addBackpackInventorySlots();

		addPlayerInventorySlots(playerInventory, yPosition);
	}

	private int addBackpackInventorySlots() {
		BackpackInventoryHandler inventoryHandler = backPackWrapper.getInventoryHandler();
		int slotIndex = 0;
		int yPosition = 18;

		while (slotIndex < inventoryHandler.getSlots()) {
			int lineIndex = slotIndex % getSlotsOnLine();
			addSlot(new SlotItemHandler(inventoryHandler, slotIndex, 8 + lineIndex * 18, yPosition));

			slotIndex++;
			if (slotIndex % getSlotsOnLine() == 0) {
				yPosition += 18;
			}
		}

		yPosition += 14;
		return yPosition;
	}

	private void addPlayerInventorySlots(PlayerInventory playerInventory, int yPosition) {
		int playerInventoryYOffset = backPackWrapper.getScreenProperties().getPlayerInventoryYOffset();

		for (int i = 0; i < 3; ++i) {
			for (int j = 0; j < 9; ++j) {
				addSlot(new Slot(playerInventory, j + i * 9 + 9, playerInventoryYOffset + 8 + j * 18, yPosition));
			}
			yPosition += 18;
		}

		yPosition += 4;

		for (int k = 0; k < 9; ++k) {
			Slot slot = addSlot(new Slot(playerInventory, k, playerInventoryYOffset + 8 + k * 18, yPosition));
			if (k == playerInventory.currentItem) {
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
		Hand hand = packetBuffer.readBoolean() ? Hand.MAIN_HAND : Hand.OFF_HAND;
		return new BackpackContainer(windowId, playerInventory, playerInventory.player.getHeldItem(hand));
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
			int numRows = getNumberOfRows();
			if (index < numRows * 9) {
				if (!mergeItemStack(slotStack, numRows * 9, inventorySlots.size(), true)) {
					return ItemStack.EMPTY;
				}
			} else if (!mergeItemStack(slotStack, 0, numRows * 9, false)) {
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

	@Override
	public ItemStack slotClick(int slotId, int dragType, ClickType clickType, PlayerEntity player) {
		if (slotId == backpackSlotNumber) {
			return ItemStack.EMPTY;
		}

		return super.slotClick(slotId, dragType, clickType, player);
	}

	public int getNumberOfSlots() {
		return backPackWrapper.getInventoryHandler().getSlots();
	}

	public ScreenProperties getScreenProperties() {
		return backPackWrapper.getScreenProperties();
	}
}
