package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.FilteredItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.Collections;
import java.util.function.Consumer;

public class RestockUpgradeWrapper extends UpgradeWrapperBase<RestockUpgradeWrapper, RestockUpgradeItem>
		implements IFilteredUpgrade, IItemHandlerInteractionUpgrade {
	private final FilterLogic filterLogic;

	public RestockUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(IItemHandler itemHandler) {
		InventoryHelper.transfer(itemHandler, new FilteredItemHandler<>(backpackWrapper.getInventoryForUpgradeProcessing(), Collections.singletonList(filterLogic), Collections.emptyList()));
	}
}
