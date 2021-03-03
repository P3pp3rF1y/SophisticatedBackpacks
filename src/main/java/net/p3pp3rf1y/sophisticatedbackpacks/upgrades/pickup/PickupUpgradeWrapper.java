package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.function.Consumer;

public class PickupUpgradeWrapper extends UpgradeWrapperBase<PickupUpgradeWrapper, PickupUpgradeItem>
		implements IPickupResponseUpgrade, IContentsFilteredUpgrade {
	private static final int BACKPACK_FILTER_REFRESH_COOLDOWN_TICKS = 10;
	private static final int FULL_COOLDOWN = 60;
	private final ContentsFilterLogic filterLogic;
	private long backpackContentsRefreshCooldown = 0;

	public PickupUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, stack -> save(), upgradeItem.getFilterSlotCount());
	}

	@Override
	public ItemStack pickup(World world, ItemStack stack, boolean simulate) {
		if (isInCooldown(world)) {
			return stack;
		}

		if (backpackContentsRefreshCooldown < world.getGameTime()) {
			backpackContentsRefreshCooldown = world.getGameTime() + BACKPACK_FILTER_REFRESH_COOLDOWN_TICKS;
			filterLogic.refreshBackpackFilterStacks(backpackWrapper.getInventoryForUpgradeProcessing());
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
	public ContentsFilterLogic getFilterLogic() {
		return filterLogic;
	}
}
