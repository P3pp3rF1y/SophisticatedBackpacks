package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

public class BackpackWrapper {
	private final ItemStack backpack;
	private BackpackInventoryHandler handler = null;

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
	}

	public BackpackInventoryHandler getInventoryHandler() {
		if (handler == null) {
			handler = new BackpackInventoryHandler(backpack);
		}
		return handler;
	}

	public ScreenProperties getScreenProperties() {
		return ((BackpackItem) backpack.getItem()).getScreenProperties();
	}

	public void copyDataTo(BackpackWrapper otherBackpackWrapper) {
		if (backpack.hasDisplayName()) {
			otherBackpackWrapper.backpack.setDisplayName(backpack.getDisplayName());
		}
		getInventoryHandler().copyStacksTo(otherBackpackWrapper.getInventoryHandler());
	}
}
