package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

public interface IBackpackWrapper {
	void linkToTileEntity(BackpackTileEntity te);

	BackpackInventoryHandler getInventoryHandler();

	IItemHandler getFilteredHandler();

	ScreenProperties getScreenProperties();

	void copyDataTo(IBackpackWrapper otherBackpackWrapper);

	BackpackUpgradeHandler getUpgradeHandler();

	int getClothColor();

	int getBorderColor();

	void setColors(int clothColor, int borderColor);

	ItemStack getBackpack();
}
