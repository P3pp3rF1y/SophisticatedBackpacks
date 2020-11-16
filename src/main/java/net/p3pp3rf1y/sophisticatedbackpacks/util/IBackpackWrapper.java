package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

public interface IBackpackWrapper {
	BackpackInventoryHandler getInventoryHandler();

	ScreenProperties getScreenProperties();

	void copyDataTo(IBackpackWrapper otherBackpackWrapper);

	BackpackUpgradeHandler getUpgradeHandler();

	int getClothColor();

	int getBorderColor();

	void setColors(int clothColor, int borderColor);

	ItemStack getBackpack();
}
