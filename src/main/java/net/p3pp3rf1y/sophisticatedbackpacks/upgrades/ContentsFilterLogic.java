package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemStackKey;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

public class ContentsFilterLogic extends FilterLogic {
	private Set<ItemStackKey> backpackFilterStacks = new HashSet<>();

	public ContentsFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount) {
		super(upgrade, saveHandler, filterSlotCount);
	}

	public ContentsFilterType getFilterType() {
		if (shouldFilterByBackpack()) {
			return ContentsFilterType.BACKPACK;
		}
		return isAllowList() ? ContentsFilterType.ALLOW : ContentsFilterType.BLOCK;
	}

	public void setDepositFilterType(ContentsFilterType contentsFilterType) {
		switch (contentsFilterType) {
			case ALLOW:
				setFilterByBackpack(false);
				setAllowList(true);
				break;
			case BLOCK:
				setFilterByBackpack(false);
				setAllowList(false);
				break;
			case BACKPACK:
			default:
				setFilterByBackpack(true);
				save();
		}
	}

	public void refreshBackpackFilterStacks(IItemHandler backpackInventory) {
		backpackFilterStacks = InventoryHelper.getUniqueStacks(backpackInventory);
	}

	@Override
	public boolean matchesFilter(ItemStack stack) {
		if (!shouldFilterByBackpack()) {
			return super.matchesFilter(stack);
		}

		for (ItemStackKey filterStack : backpackFilterStacks) {
			if (stackMatchesFilter(stack, filterStack.getStack())) {
				return true;
			}
		}
		return false;
	}

	private void setFilterByBackpack(boolean filterByBackpack) {
		NBTHelper.setBoolean(upgrade, "filterByBackpack", filterByBackpack);
		save();
	}

	private boolean shouldFilterByBackpack() {
		return NBTHelper.getBoolean(upgrade, "filterByBackpack").orElse(false);
	}
}
