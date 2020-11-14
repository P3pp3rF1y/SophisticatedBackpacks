package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.LongNBT;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class PickupUpgradeItem extends ItemBase implements IBackpackUpgrade, IPickupResponseUpgrade {
	private static final int FULL_COOLDOWN = 60;

	private final int filterSlotCount;

	public PickupUpgradeItem(String regName) {
		this(regName, 9);
	}

	public PickupUpgradeItem(String regName, int filterSlotCount) {
		super(regName, new Properties().maxStackSize(1));
		this.filterSlotCount = filterSlotCount;
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
		return NBTHelper.getLong(backpack, "cooldownTime").orElse(0L);
	}

	public int getFilterSlotCount() {
		return filterSlotCount;
	}
}
