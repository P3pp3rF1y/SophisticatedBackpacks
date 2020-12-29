package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.Item;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class ItemBase extends Item {
	public ItemBase(Properties properties) {
		super(properties.group(SophisticatedBackpacks.ITEM_GROUP));
	}
}
