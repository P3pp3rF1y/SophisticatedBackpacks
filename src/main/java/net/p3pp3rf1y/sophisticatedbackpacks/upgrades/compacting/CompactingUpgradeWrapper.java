package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting;

import net.minecraft.entity.LivingEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ISlotChangeResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper.CompactingShape;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

public class CompactingUpgradeWrapper extends UpgradeWrapperBase<CompactingUpgradeWrapper, CompactingUpgradeItem>
		implements IInsertResponseUpgrade, IFilteredUpgrade, ISlotChangeResponseUpgrade, ITickableUpgrade {
	private final FilterLogic filterLogic;
	private final Set<Integer> slotsToCompact = new HashSet<>();

	public CompactingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);

		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(),
				stack -> !stack.hasTag() && !RecipeHelper.getItemCompactingShapes(stack.getItem()).isEmpty());
	}

	@Override
	public ItemStack onBeforeInsert(IItemHandler inventoryHandler, int slot, ItemStack stack, boolean simulate) {
		return stack;
	}

	@Override
	public void onAfterInsert(IItemHandler inventoryHandler, int slot) {
		compactSlot(inventoryHandler, slot);
	}

	private void compactSlot(IItemHandler inventoryHandler, int slot) {
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

	private void tryCompacting(IItemHandler inventoryHandler, Item item, int width, int height) {
		int totalCount = width * height;
		RecipeHelper.CompactingResult compactingResult = RecipeHelper.getCompactingResult(item, width, height);
		if (!compactingResult.getResult().isEmpty()) {
			ItemStack extractedStack = InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, true);
			while (extractedStack.getCount() == totalCount && fitsResultAndRemainingItems(inventoryHandler, compactingResult.getRemainingItems(), compactingResult.getResult())) {
				InventoryHelper.extractFromInventory(item, totalCount, inventoryHandler, false);
				InventoryHelper.insertIntoInventory(compactingResult.getResult(), inventoryHandler, false);
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
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (slotsToCompact.isEmpty()) {
			return;
		}

		for (int slot : slotsToCompact) {
			compactSlot(backpackWrapper.getInventoryHandler(), slot);
		}

		slotsToCompact.clear();
	}
}
