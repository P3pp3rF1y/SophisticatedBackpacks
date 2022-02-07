package net.p3pp3rf1y.sophisticatedcore.common.gui;

import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedcore.api.ISlotChangeResponseUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;

public class StorageInventorySlot extends SlotItemHandler {
	private final boolean isClientSide;
	private final IStorageWrapper storageWrapper;
	private final InventoryHandler inventoryHandler;
	private final int slotIndex;

	public StorageInventorySlot(boolean isClientSide, IStorageWrapper storageWrapper, InventoryHandler inventoryHandler, int slotIndex, int lineIndex, int yPosition) {
		super(inventoryHandler, slotIndex, 8 + lineIndex * 18, yPosition);
		this.isClientSide = isClientSide;
		this.storageWrapper = storageWrapper;
		this.inventoryHandler = inventoryHandler;
		this.slotIndex = slotIndex;
	}

	@Override
	public void setChanged() {
		super.setChanged();
		// saving here as well because there are many cases where vanilla modifies stack directly without and inventory handler isn't aware of it
		// however it does notify the slot of change
		storageWrapper.getInventoryHandler().onContentsChanged(slotIndex);
		processSlotChangeResponse(slotIndex, storageWrapper.getInventoryHandler(), storageWrapper);
	}

	private void processSlotChangeResponse(int slot, IItemHandler handler, IStorageWrapper storageWrapper) {
		if (!isClientSide) {
			storageWrapper.getUpgradeHandler().getWrappersThatImplementFromMainStorage(ISlotChangeResponseUpgrade.class).forEach(u -> u.onSlotChange(handler, slot));
		}
	}

	@Override
	public int getMaxStackSize(ItemStack stack) {
		return inventoryHandler.getStackLimit(slotIndex, stack);
	}
}
