package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;

public class InventoryHelper {
	private InventoryHelper() {}

	public static void copyTo(IItemHandlerModifiable handler, IItemHandlerModifiable otherHandler) {
		for (int slot = 0; slot < handler.getSlots() && slot < otherHandler.getSlots(); slot++) {
			ItemStack slotStack = handler.getStackInSlot(slot);
			if (!slotStack.isEmpty()) {
				otherHandler.setStackInSlot(slot, slotStack);
			}
		}
	}

	public static ItemStack insertIntoInventory(ItemStack stack, IItemHandler inventory, boolean simulate) {
		ItemStack remainingStack = stack.copy();
		for (int slot = 0; slot < inventory.getSlots() && !remainingStack.isEmpty(); slot++) {
			remainingStack = inventory.insertItem(slot, remainingStack, simulate);
		}
		return remainingStack;
	}
}
