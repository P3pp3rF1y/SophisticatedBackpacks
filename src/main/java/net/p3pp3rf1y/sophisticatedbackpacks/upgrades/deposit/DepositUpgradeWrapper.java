package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.FilteredItemHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

public class DepositUpgradeWrapper extends UpgradeWrapperBase<DepositUpgradeWrapper, DepositUpgradeItem>
		implements IFilteredUpgrade, IItemHandlerInteractionUpgrade {
	private final DepositFilterLogic filterLogic;

	public DepositUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new DepositFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(), ModCoreDataComponents.FILTER_ATTRIBUTES);
	}

	@Override
	public DepositFilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(IItemHandler itemHandler, Player player) {
		List<ItemStack> transferredStacks = depositToHandler(itemHandler);

		int stacksDeposited = transferredStacks.size();
		String translKey = stacksDeposited > 0 ? "gui.sophisticatedbackpacks.status.stacks_deposited" : "gui.sophisticatedbackpacks.status.nothing_to_deposit";
		player.displayClientMessage(Component.translatable(translKey, stacksDeposited), true);
	}

	public List<ItemStack> depositToHandler(IItemHandler itemHandler) {
		if (filterLogic.getDepositFilterType() == DepositFilterType.INVENTORY) {
			filterLogic.setInventory(itemHandler);
		}
		List<ItemStack> transferredStacks = new ArrayList<>();

		InventoryHelper.transfer(storageWrapper.getInventoryForUpgradeProcessing(),
				new FilteredItemHandler<>(itemHandler, Collections.singletonList(filterLogic), Collections.emptyList()),
				s -> transferredStacks.add(s.get()));

		return transferredStacks;
	}
}
