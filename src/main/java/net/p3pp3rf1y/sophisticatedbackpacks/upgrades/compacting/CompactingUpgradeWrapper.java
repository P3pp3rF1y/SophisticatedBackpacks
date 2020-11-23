package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.CraftingHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.CraftingHelper.CompactingShape;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.function.Consumer;

public class CompactingUpgradeWrapper extends UpgradeWrapperBase<CompactingUpgradeWrapper, CompactingUpgradeItem>
		implements IInsertResponseUpgrade, IFilteredUpgrade {
	private final FilterLogic filterLogic;

	public CompactingUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);

		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(),
				stack -> CraftingHelper.getItemCompactingShape(stack.getItem()) != CompactingShape.NONE);
	}

	@Override
	public void onInsert(BackpackInventoryHandler inventoryHandler, int slot) {
		ItemStack slotStack = inventoryHandler.getStackInSlot(slot);

		if (!filterLogic.matchesFilter(slotStack)) {
			return;
		}

		Item item = slotStack.getItem();

		CompactingShape shape = CraftingHelper.getItemCompactingShape(item);

		if (shape == CompactingShape.TWO_BY_TWO) {
			tryCompacting(inventoryHandler, item, 2, 2);
		} else if (upgradeItem.shouldCompactNineByNine() && shape == CompactingShape.THREE_BY_THREE) {
			tryCompacting(inventoryHandler, item, 3, 3);
		}
	}

	private void tryCompacting(BackpackInventoryHandler inventoryHandler, Item item, int width, int height) {
		int totalCount = width * height;
		ItemStack result = CraftingHelper.getCraftingResult(item, width, height);
		if (!result.isEmpty()) {
			ItemStack extractedStack = InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, true);
			while (extractedStack.getCount() == totalCount && InventoryHelper.insertIntoInventory(result, inventoryHandler, true).isEmpty()) {
				InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, false);
				InventoryHelper.insertIntoInventory(result, inventoryHandler, false);
				extractedStack = InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, true);
			}
		}
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}
}
