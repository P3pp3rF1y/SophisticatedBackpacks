package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
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
		implements IContentsFilteredUpgrade, IItemHandlerInteractionUpgrade {
	private final ContentsFilterLogic filterLogic;

	public RestockUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(),
				backpackWrapper::getInventoryHandler, backpackWrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class),
				ModCoreDataComponents.FILTER_ATTRIBUTES);
	}

	@Override
	public ContentsFilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(IItemHandler itemHandler, Player player) {
		List<ItemStack> transferredStacks = restockFromHandler(itemHandler);

		int stacksRestocked = transferredStacks.size();
		String translKey = stacksRestocked > 0 ? "gui.sophisticatedbackpacks.status.stacks_restocked" : "gui.sophisticatedbackpacks.status.nothing_to_restock";
		player.displayClientMessage(Component.translatable(translKey, stacksRestocked), true);
	}

	public List<ItemStack> restockFromHandler(IItemHandler itemHandler) {
		List<ItemStack> transferredStacks = new ArrayList<>();

		InventoryHelper.transfer(itemHandler,
				new FilteredItemHandler<>(storageWrapper.getInventoryForUpgradeProcessing(), Collections.singletonList(filterLogic), Collections.emptyList()),
				s -> transferredStacks.add(s.get()));

		return transferredStacks;
	}
}
