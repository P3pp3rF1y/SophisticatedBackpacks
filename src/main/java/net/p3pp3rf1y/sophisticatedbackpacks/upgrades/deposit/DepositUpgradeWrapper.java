package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemResourceHandlerInteractionUpgrade;
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
		implements IFilteredUpgrade, IItemResourceHandlerInteractionUpgrade {
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
	public void onHandlerInteract(ResourceHandler<ItemResource> handler, Player player) {
		List<ItemStack> transferredStacks = depositToHandler(handler);

		int stacksDeposited = transferredStacks.size();
		String translKey = stacksDeposited > 0 ? "gui.sophisticatedbackpacks.status.stacks_deposited" : "gui.sophisticatedbackpacks.status.nothing_to_deposit";
		player.sendOverlayMessage(Component.translatable(translKey, stacksDeposited));
	}

	public List<ItemStack> depositToHandler(ResourceHandler<ItemResource> handler) {
		if (filterLogic.getDepositFilterType() == DepositFilterType.INVENTORY) {
			filterLogic.setInventory(handler);
		}
		List<ItemStack> transferredStacks = new ArrayList<>();

		try (Transaction tx = Transaction.openRoot()) {
			FilteredItemHandler<ResourceHandler<ItemResource>> filteredTarget = new FilteredItemHandler<>(handler, Collections.singletonList(filterLogic), Collections.emptyList());
			InventoryHelper.iterate(storageWrapper.getInventoryForUpgradeProcessing(), (index, resource, amount) -> {
				if (resource.isEmpty()) {
					return;
				}
				int moved = filteredTarget.insert(resource, amount, tx);
				if (moved > 0) {
					storageWrapper.getInventoryForUpgradeProcessing().extract(index, resource, moved, tx);
					transferredStacks.add(resource.toStack(moved));
				}
			});
			tx.commit();
		}

		return transferredStacks;
	}
}
