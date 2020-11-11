package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

public interface IPickupResponseUpgrade {
	ItemStack pickup(World world, ItemStack upgrade, ItemStack stack, BackpackWrapper backpack, boolean simulate);

	long getCooldownTime(ItemStack stack);
}
