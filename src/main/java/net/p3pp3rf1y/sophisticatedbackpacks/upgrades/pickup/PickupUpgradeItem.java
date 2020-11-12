package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.LongNBT;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

public class PickupUpgradeItem extends ItemBase implements IBackpackUpgrade, IPickupResponseUpgrade {
	private static final int FULL_COOLDOWN = 60;

	public PickupUpgradeItem() {
		super("pickup_upgrade", new Properties().maxStackSize(1));
	}

	@Override
	public ItemStack pickup(World world, ItemStack upgrade, ItemStack stack, BackpackWrapper backpackWrapper, boolean simulate) {
		PickupUpgradeWrapper pickupWrapper = new PickupUpgradeWrapper(upgrade);
		if (!pickupWrapper.matchesFilter(stack)) {
			return stack;
		}
		int originalCount = stack.getCount();
		ItemStack ret = InventoryHelper.insertIntoInventory(stack, backpackWrapper.getInventoryHandler(), simulate);
		if (originalCount == ret.getCount()) {
			setCooldown(backpackWrapper.getBackpack(), world.getGameTime() + FULL_COOLDOWN);
		}

		return ret;
	}

	private void setCooldown(ItemStack backpack, long time) {
		backpack.setTagInfo("cooldownTime", LongNBT.valueOf(time));
	}

	@Override
	public long getCooldownTime(ItemStack backpack) {
		return backpack.hasTag() ? backpack.getTag().getLong("cooldownTime") : 0;
	}
}
