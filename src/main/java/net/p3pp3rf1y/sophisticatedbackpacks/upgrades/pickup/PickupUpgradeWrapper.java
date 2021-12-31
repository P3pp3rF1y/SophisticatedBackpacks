package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;

import java.util.function.Consumer;

public class PickupUpgradeWrapper extends UpgradeWrapperBase<PickupUpgradeWrapper, PickupUpgradeItem>
		implements IPickupResponseUpgrade, IContentsFilteredUpgrade {
	private final ContentsFilterLogic filterLogic;

	public PickupUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, stack -> save(), upgradeItem.getFilterSlotCount(), backpackWrapper::getInventoryHandler, backpackWrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class));
	}

	@Override
	public ItemStack pickup(World world, ItemStack stack, boolean simulate) {
		if (!filterLogic.matchesFilter(stack)) {
			return stack;
		}

		return backpackWrapper.getInventoryForUpgradeProcessing().insertItem(stack, simulate);
	}

	@Override
	public ContentsFilterLogic getFilterLogic() {
		return filterLogic;
	}
}
