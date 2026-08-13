package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

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
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

public class RestockUpgradeWrapper extends UpgradeWrapperBase<RestockUpgradeWrapper, RestockUpgradeItem>
		implements
			IContentsFilteredUpgrade,
			IItemResourceHandlerInteractionUpgrade {
	private final ContentsFilterLogic filterLogic;

	public RestockUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(), backpackWrapper::getInventoryHandler,
				backpackWrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class), ModCoreDataComponents.FILTER_ATTRIBUTES);
	}

	@Override
	public ContentsFilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(ResourceHandler<ItemResource> handler, Player player) {
		List<ItemStack> transferredStacks = restockFromHandler(handler);

		int stacksRestocked = transferredStacks.size();
		String translKey = stacksRestocked > 0 ? "gui.sophisticatedbackpacks.status.stacks_restocked" : "gui.sophisticatedbackpacks.status.nothing_to_restock";
		player.sendOverlayMessage(Component.translatable(translKey, stacksRestocked));
	}

	public List<ItemStack> restockFromHandler(ResourceHandler<ItemResource> handler) {
		List<ItemStack> transferredStacks = new ArrayList<>();

		try (Transaction tx = Transaction.openRoot()) {
			FilteredItemHandler<ResourceHandler<ItemResource>> filteredTarget = new FilteredItemHandler<>(storageWrapper.getInventoryForUpgradeProcessing(),
					Collections.singletonList(filterLogic), Collections.emptyList());
			InventoryHelper.iterate(handler, (index, resource, amount) -> {
				if (!filterLogic.matchesFilter(resource)) {
					return;
				}

				int amountToMove;
				try (Transaction probeTx = Transaction.open(tx)) {
					amountToMove = filteredTarget.insert(resource, amount, probeTx);
				}
				if (amountToMove <= 0) {
					return;
				}

				try (Transaction transferTx = Transaction.open(tx)) {
					int extracted = handler.extract(index, resource, amountToMove, transferTx);
					if (extracted <= 0) {
						return;
					}

					int inserted = filteredTarget.insert(resource, extracted, transferTx);
					if (inserted != extracted) {
						return;
					}

					transferTx.commit();
					transferredStacks.add(resource.toStack(extracted));
				}
			});
			if (!transferredStacks.isEmpty()) {
				tx.commit();
			}
		}

		return transferredStacks;
	}
}
