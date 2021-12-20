package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.world.item.ItemStack;

public interface IOverflowResponseUpgrade {
	boolean worksInGui();

	ItemStack onOverflow(ItemStack stack);
}
