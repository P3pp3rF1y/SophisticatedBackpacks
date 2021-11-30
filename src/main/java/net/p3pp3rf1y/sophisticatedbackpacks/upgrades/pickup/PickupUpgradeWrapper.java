package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;

import java.util.function.Consumer;

public class PickupUpgradeWrapper extends UpgradeWrapperBase<PickupUpgradeWrapper, PickupUpgradeItem>
		implements IPickupResponseUpgrade, IContentsFilteredUpgrade {
	private static final int FULL_COOLDOWN = 60;
	private final ContentsFilterLogic filterLogic;

	public PickupUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, stack -> save(), upgradeItem.getFilterSlotCount(), backpackWrapper::getInventoryHandler);
	}

	@Override
	public ItemStack pickup(Level world, ItemStack stack, boolean simulate) {
		if (isInCooldown(world)) {
			return stack;
		}

		if (!filterLogic.matchesFilter(stack)) {
			return stack;
		}

		int originalCount = stack.getCount();
		ItemStack ret = backpackWrapper.getInventoryForUpgradeProcessing().insertItem(stack, simulate);
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
