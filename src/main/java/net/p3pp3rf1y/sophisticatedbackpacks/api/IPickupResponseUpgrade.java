package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

public interface IPickupResponseUpgrade {
	ItemStack pickup(ItemStack stack, BackpackWrapper backpack, boolean simulate);
}
