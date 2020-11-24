package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackInventoryHandler;

public interface IInsertResponseUpgrade {
	ItemStack onBeforeInsert(BackpackInventoryHandler inventoryHandler, int slot, ItemStack stack, boolean simulate);

	void onAfterInsert(BackpackInventoryHandler inventoryHandler, int slot);
}
