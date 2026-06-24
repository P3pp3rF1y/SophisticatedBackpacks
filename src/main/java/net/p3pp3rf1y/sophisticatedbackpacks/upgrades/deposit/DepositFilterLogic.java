package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.core.component.DataComponentType;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterAttributes;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

public class DepositFilterLogic extends FilterLogic {
	private Set<ItemStackKey> inventoryFilterStacks = new HashSet<>();

	public DepositFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount,
			DeferredHolder<DataComponentType<?>, DataComponentType<FilterAttributes>> contentsComponent) {
		super(upgrade, saveHandler, filterSlotCount, contentsComponent);
	}

	public DepositFilterType getDepositFilterType() {
		if (shouldFilterByInventory()) {
			return DepositFilterType.INVENTORY;
		}
		return isAllowList() ? DepositFilterType.ALLOW : DepositFilterType.BLOCK;
	}

	public void setDepositFilterType(DepositFilterType depositFilterType) {
		switch (depositFilterType) {
			case ALLOW :
				setFilterByInventory(false);
				setAllowList(true);
				break;
			case BLOCK :
				setFilterByInventory(false);
				setAllowList(false);
				break;
			case INVENTORY :
			default :
				setFilterByInventory(true);
				save();
		}
	}

	public void setInventory(IItemHandler inventory) {
		inventoryFilterStacks = InventoryHelper.getUniqueStacks(inventory);
	}

	@Override
	public boolean matchesFilter(ItemStack stack) {
		if (!shouldFilterByInventory()) {
			return super.matchesFilter(stack);
		}

		for (ItemStackKey filterStack : inventoryFilterStacks) {
			if (stackMatchesFilter(stack, filterStack.getStack())) {
				return true;
			}
		}
		return false;
	}

	private void setFilterByInventory(boolean filterByInventory) {
		upgrade.set(ModDataComponents.FILTER_BY_INVENTORY, filterByInventory);
		save();
	}

	private boolean shouldFilterByInventory() {
		return upgrade.getOrDefault(ModDataComponents.FILTER_BY_INVENTORY, false);
	}
}
