package net.p3pp3rf1y.sophisticatedcore.upgrades.compacting;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedcore.api.ISlotChangeResponseUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IInsertResponseUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RecipeHelper.CompactingShape;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

public class CompactingUpgradeWrapper extends UpgradeWrapperBase<CompactingUpgradeWrapper, CompactingUpgradeItem>
		implements IInsertResponseUpgrade, IFilteredUpgrade, ISlotChangeResponseUpgrade, ITickableUpgrade {
	private final FilterLogic filterLogic;
	private final Set<Integer> slotsToCompact = new HashSet<>();

	public CompactingUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);

		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(),
				stack -> !stack.hasTag() && !RecipeHelper.getItemCompactingShapes(stack.getItem()).isEmpty());
	}

	@Override
	public ItemStack onBeforeInsert(IItemHandlerSimpleInserter inventoryHandler, int slot, ItemStack stack, boolean simulate) {
		return stack;
	}

	@Override
	public void onAfterInsert(IItemHandlerSimpleInserter inventoryHandler, int slot) {
		compactSlot(inventoryHandler, slot);
	}

	private void compactSlot(IItemHandlerSimpleInserter inventoryHandler, int slot) {
		ItemStack slotStack = inventoryHandler.getStackInSlot(slot);

		if (slotStack.isEmpty() || slotStack.hasTag() || !filterLogic.matchesFilter(slotStack)) {
			return;
		}

		Item item = slotStack.getItem();

		Set<CompactingShape> shapes = RecipeHelper.getItemCompactingShapes(item);

		if (upgradeItem.shouldCompactThreeByThree() && (shapes.contains(CompactingShape.THREE_BY_THREE_UNCRAFTABLE) || (shouldCompactNonUncraftable() && shapes.contains(CompactingShape.THREE_BY_THREE)))) {
			tryCompacting(inventoryHandler, item, 3, 3);
		} else if (shapes.contains(CompactingShape.TWO_BY_TWO_UNCRAFTABLE) || (shouldCompactNonUncraftable() && shapes.contains(CompactingShape.TWO_BY_TWO))) {
			tryCompacting(inventoryHandler, item, 2, 2);
		}
	}

	private void tryCompacting(IItemHandlerSimpleInserter inventoryHandler, Item item, int width, int height) {
		int totalCount = width * height;
		RecipeHelper.CompactingResult compactingResult = RecipeHelper.getCompactingResult(item, width, height);
		if (!compactingResult.getResult().isEmpty()) {
			ItemStack extractedStack = InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, true);
			while (extractedStack.getCount() == totalCount && fitsResultAndRemainingItems(inventoryHandler, compactingResult.getRemainingItems(), compactingResult.getResult())) {
				InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, false);
				inventoryHandler.insertItem(compactingResult.getResult(), false);
				InventoryHelper.insertIntoInventory(compactingResult.getRemainingItems(), inventoryHandler, false);
				extractedStack = InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, true);
			}
		}
	}

	private boolean fitsResultAndRemainingItems(IItemHandler inventoryHandler, List<ItemStack> remainingItems, ItemStack result) {
		if (!remainingItems.isEmpty()) {
			IItemHandler clonedHandler = InventoryHelper.cloneInventory(inventoryHandler);
			return InventoryHelper.insertIntoInventory(result, clonedHandler, false).isEmpty()
					&& InventoryHelper.insertIntoInventory(remainingItems, clonedHandler, false).isEmpty();
		}
		return InventoryHelper.insertIntoInventory(result, inventoryHandler, true).isEmpty();
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	public boolean shouldCompactNonUncraftable() {
		return NBTHelper.getBoolean(upgrade, "compactNonUncraftable").orElse(false);
	}

	public void setCompactNonUncraftable(boolean shouldCompactNonUncraftable) {
		NBTHelper.setBoolean(upgrade, "compactNonUncraftable", shouldCompactNonUncraftable);
		save();
	}

	@Override
	public void onSlotChange(IItemHandler inventoryHandler, int slot) {
		if (shouldWorkInGUI()) {
			slotsToCompact.add(slot);
		}
	}

	public void setShouldWorkdInGUI(boolean shouldWorkdInGUI) {
		NBTHelper.setBoolean(upgrade, "shouldWorkInGUI", shouldWorkdInGUI);
		save();
	}

	public boolean shouldWorkInGUI() {
		return NBTHelper.getBoolean(upgrade, "shouldWorkInGUI").orElse(false);
	}

	@Override
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (slotsToCompact.isEmpty()) {
			return;
		}

		for (int slot : slotsToCompact) {
			compactSlot(storageWrapper.getInventoryHandler(), slot);
		}

		slotsToCompact.clear();
	}
}
