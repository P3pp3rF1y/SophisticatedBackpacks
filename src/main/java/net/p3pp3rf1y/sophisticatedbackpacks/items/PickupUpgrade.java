package net.p3pp3rf1y.sophisticatedbackpacks.items;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

public class PickupUpgrade extends ItemBase implements IBackpackUpgrade, IPickupResponseUpgrade {
	public PickupUpgrade() {
		super("pickup_upgrade", new Properties().maxStackSize(1));
	}

	@Override
	public ItemStack pickup(ItemStack stack, BackpackWrapper backpackWrapper, boolean simulate) {
		return InventoryHelper.insertIntoInventory(stack, backpackWrapper.getInventoryHandler(), simulate);
	}
}
