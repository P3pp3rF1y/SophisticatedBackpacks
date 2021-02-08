package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraftforge.items.IItemHandler;

public interface ISlotChangeResponseUpgrade {
	void onSlotChange(IItemHandler inventoryHandler, int slot);
}
