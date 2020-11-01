package net.p3pp3rf1y.sophisticatedbackpacks.items;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.LongNBT;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

public class PickupUpgrade extends ItemBase implements IBackpackUpgrade, IPickupResponseUpgrade {
	private static final int FULL_COOLDOWN = 60;

	public PickupUpgrade() {
		super("pickup_upgrade", new Properties().maxStackSize(1));
	}

	@Override
	public ItemStack pickup(World world, ItemStack stack, BackpackWrapper backpackWrapper, boolean simulate) {
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
