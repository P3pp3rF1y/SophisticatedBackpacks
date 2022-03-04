package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.world.item.ItemStack;

public interface IOverflowResponseUpgrade {
	boolean worksInGui();

	ItemStack onOverflow(ItemStack stack);

	boolean stackMatchesFilter(ItemStack stack);
}
