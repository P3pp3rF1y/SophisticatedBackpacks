package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

public class NoopBackpackWrapper implements IBackpackWrapper {
	public static final NoopBackpackWrapper INSTANCE = new NoopBackpackWrapper();

	private NoopBackpackWrapper() {}

	private final ItemStack backpack = new ItemStack(ModItems.BACKPACK);

	@Override
	public BackpackInventoryHandler getInventoryHandler() {
		return new BackpackInventoryHandler(backpack, s -> {});
	}

	@Override
	public ScreenProperties getScreenProperties() {
		return new ScreenProperties();
	}

	@Override
	public void copyDataTo(IBackpackWrapper otherBackpackWrapper) {
		//noop
	}

	@Override
	public BackpackUpgradeHandler getUpgradeHandler() {
		return new BackpackUpgradeHandler(backpack, s -> {});
	}

	@Override
	public int getClothColor() {
		return -1;
	}

	@Override
	public int getBorderColor() {
		return -1;
	}

	@Override
	public void setColors(int clothColor, int borderColor) {
		//noop
	}

	@Override
	public ItemStack getBackpack() {
		return backpack;
	}
}
