package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;

public interface IPickupResponseUpgrade {
	ItemStack pickup(ItemStack stack, ItemStack backpack, boolean simulate);
}
