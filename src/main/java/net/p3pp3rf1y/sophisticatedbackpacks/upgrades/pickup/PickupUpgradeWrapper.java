package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.function.Consumer;

public class PickupUpgradeWrapper extends UpgradeWrapperBase<PickupUpgradeWrapper, PickupUpgradeItem> implements IPickupResponseUpgrade, IFilteredUpgrade {
	private static final int FULL_COOLDOWN = 60;
	private final FilterLogic filterLogic;

	public PickupUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, stack -> save(), upgradeItem.getFilterSlotCount());
	}

	@Override
	public ItemStack pickup(World world, ItemStack stack, IBackpackWrapper backpackWrapper, boolean simulate) {
		if (isInCooldown(world)) {
			return stack;
		}

		if (!filterLogic.matchesFilter(stack)) {
			return stack;
		}

		int originalCount = stack.getCount();
		ItemStack ret = InventoryHelper.insertIntoInventory(stack, backpackWrapper.getInventoryForUpgradeProcessing(), simulate);
		if (originalCount == ret.getCount()) {
			setCooldown(world, FULL_COOLDOWN);
		}

		return ret;
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}
}
