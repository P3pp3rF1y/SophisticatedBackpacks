package net.p3pp3rf1y.sophisticatedbackpacks.items;

import net.minecraft.item.Item;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class ItemBase extends Item {
	public ItemBase(String regName) {
		this(regName, new Properties());
	}

	public ItemBase(String regName, Properties properties) {
		super(properties.group(SophisticatedBackpacks.ITEM_GROUP));
		setRegistryName(SophisticatedBackpacks.MOD_ID, regName);
	}
}
