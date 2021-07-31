package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ISlotChangeResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;

public class BackpackInventorySlot extends SlotItemHandler {
	private final boolean isClientSide;
	private final IBackpackWrapper backpackWrapper;
	private final BackpackInventoryHandler inventoryHandler;
	private final int slotIndex;

	public BackpackInventorySlot(boolean isClientSide, IBackpackWrapper backpackWrapper, BackpackInventoryHandler inventoryHandler, int slotIndex, int lineIndex, int yPosition) {
		super(inventoryHandler, slotIndex, 8 + lineIndex * 18, yPosition);
		this.isClientSide = isClientSide;
		this.backpackWrapper = backpackWrapper;
		this.inventoryHandler = inventoryHandler;
		this.slotIndex = slotIndex;
	}

	@Override
	public void setChanged() {
		super.setChanged();
		// saving here as well because there are many cases where vanilla modifies stack directly without and inventory handler isn't aware of it
		// however it does notify the slot of change
		backpackWrapper.getInventoryHandler().onContentsChanged(slotIndex);
		processSlotChangeResponse(slotIndex, backpackWrapper.getInventoryHandler(), backpackWrapper);
	}

	private void processSlotChangeResponse(int slot, IItemHandler handler, IBackpackWrapper backpackWrapper) {
		if (!isClientSide) {
			backpackWrapper.getUpgradeHandler().getWrappersThatImplementFromMainBackpack(ISlotChangeResponseUpgrade.class).forEach(u -> u.onSlotChange(handler, slot));
		}
	}

	@Override
	public int getMaxStackSize(ItemStack stack) {
		return inventoryHandler.getStackLimit(slotIndex, stack);
	}
}
