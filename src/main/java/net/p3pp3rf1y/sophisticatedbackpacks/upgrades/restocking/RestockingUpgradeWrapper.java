package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restocking;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.FilteredItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.Collections;
import java.util.function.Consumer;

public class RestockingUpgradeWrapper extends UpgradeWrapperBase<RestockingUpgradeWrapper, RestockingUpgradeItem>
		implements IFilteredUpgrade, IItemHandlerInteractionUpgrade {
	private final FilterLogic filterLogic;

	public RestockingUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(IBackpackWrapper wrapper, IItemHandler itemHandler) {
		InventoryHelper.transfer(itemHandler, new FilteredItemHandler(wrapper.getInventoryHandler(), Collections.singletonList(filterLogic), Collections.emptyList()));
	}
}
