package net.p3pp3rf1y.sophisticatedcore.util;

import net.minecraft.core.NonNullList;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.Config;

public class ItemBase extends Item {
	public ItemBase(Properties properties, CreativeModeTab itemGroup) {
		super(properties.tab(itemGroup));
	}

	@Override
	public void fillItemCategory(CreativeModeTab group, NonNullList<ItemStack> items) {
		if (Config.COMMON.enabledItems.isItemEnabled(this)) {
			super.fillItemCategory(group, items);
		}
	}
}
