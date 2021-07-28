package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.Item;
import net.minecraft.item.ItemGroup;
import net.minecraft.item.ItemStack;
import net.minecraft.util.NonNullList;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class ItemBase extends Item {
	public ItemBase(Properties properties) {
		super(properties.group(SophisticatedBackpacks.ITEM_GROUP));
	}

	@Override
	public void fillItemGroup(ItemGroup group, NonNullList<ItemStack> items) {
		if (Config.COMMON.enabledItems.isItemEnabled(this)) {
			super.fillItemGroup(group, items);
		}
	}
}
