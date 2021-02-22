package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.FilteredItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

public class RestockUpgradeWrapper extends UpgradeWrapperBase<RestockUpgradeWrapper, RestockUpgradeItem>
		implements IFilteredUpgrade, IItemHandlerInteractionUpgrade {
	private final RestockFilterLogic filterLogic;

	public RestockUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new RestockFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	@Override
	public RestockFilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onHandlerInteract(IItemHandler itemHandler, PlayerEntity player) {
		filterLogic.refreshBackpackFilterStacks(backpackWrapper.getInventoryForUpgradeProcessing());
		AtomicInteger stacksAdded = new AtomicInteger(0);

		InventoryHelper.transfer(itemHandler,
				new FilteredItemHandler<>(backpackWrapper.getInventoryForUpgradeProcessing(), Collections.singletonList(filterLogic), Collections.emptyList()),
				s -> stacksAdded.incrementAndGet());

		int stacksRestocked = stacksAdded.get();
		String translKey = stacksRestocked > 0 ? "gui.sophisticatedbackpacks.status.stacks_restocked" : "gui.sophisticatedbackpacks.status.nothing_to_restock";
		player.sendStatusMessage(new TranslationTextComponent(translKey, stacksRestocked), true);
	}
}
