package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IItemHandlerSimpleInserter;

public interface IInsertResponseUpgrade {
	ItemStack onBeforeInsert(IItemHandlerSimpleInserter inventoryHandler, int slot, ItemStack stack, boolean simulate);

	void onAfterInsert(IItemHandlerSimpleInserter inventoryHandler, int slot);
}
