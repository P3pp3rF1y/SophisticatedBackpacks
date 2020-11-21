package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;

public interface IPickupResponseUpgrade {
	ItemStack pickup(World world, ItemStack stack, IBackpackWrapper backpack, boolean simulate);
}
