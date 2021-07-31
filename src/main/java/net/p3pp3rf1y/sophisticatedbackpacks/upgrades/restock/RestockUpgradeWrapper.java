package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.FilteredItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

public class RestockUpgradeWrapper extends UpgradeWrapperBase<RestockUpgradeWrapper, RestockUpgradeItem>
		implements IContentsFilteredUpgrade, IItemHandlerInteractionUpgrade {
	private final ContentsFilterLogic filterLogic;

	public RestockUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	@Override
	public ContentsFilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(IItemHandler itemHandler, PlayerEntity player) {
		if (filterLogic.getFilterType() == ContentsFilterType.BACKPACK) {
			filterLogic.refreshBackpackFilterStacks(backpackWrapper.getInventoryForUpgradeProcessing());
		}
		AtomicInteger stacksAdded = new AtomicInteger(0);

		InventoryHelper.transfer(itemHandler,
				new FilteredItemHandler<>(backpackWrapper.getInventoryForUpgradeProcessing(), Collections.singletonList(filterLogic), Collections.emptyList()),
				s -> stacksAdded.incrementAndGet());

		int stacksRestocked = stacksAdded.get();
		String translKey = stacksRestocked > 0 ? "gui.sophisticatedbackpacks.status.stacks_restocked" : "gui.sophisticatedbackpacks.status.nothing_to_restock";
		player.displayClientMessage(new TranslationTextComponent(translKey, stacksRestocked), true);
	}
}
