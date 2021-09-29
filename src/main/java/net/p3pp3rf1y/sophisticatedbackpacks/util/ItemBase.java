package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.core.NonNullList;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class ItemBase extends Item {
	public ItemBase(Properties properties) {
		super(properties.tab(SophisticatedBackpacks.ITEM_GROUP));
	}

	@Override
	public void fillItemCategory(CreativeModeTab group, NonNullList<ItemStack> items) {
		if (Config.COMMON.enabledItems.isItemEnabled(this)) {
			super.fillItemCategory(group, items);
		}
	}
}
