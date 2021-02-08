package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.FilteredItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.Collections;
import java.util.function.Consumer;

public class DepositUpgradeWrapper extends UpgradeWrapperBase<DepositUpgradeWrapper, DepositUpgradeItem>
		implements IFilteredUpgrade, IItemHandlerInteractionUpgrade {
	private final DepositFilterLogic filterLogic;

	public DepositUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new DepositFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	@Override
	public DepositFilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(IItemHandler itemHandler) {
		filterLogic.setInventory(itemHandler);
		InventoryHelper.transfer(backpackWrapper.getInventoryForUpgradeProcessing(), new FilteredItemHandler<>(itemHandler, Collections.singletonList(filterLogic), Collections.emptyList()));
	}
}
