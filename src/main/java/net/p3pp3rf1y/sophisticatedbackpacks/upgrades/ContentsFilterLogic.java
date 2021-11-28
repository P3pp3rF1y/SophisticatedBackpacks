package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemStackKey;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class ContentsFilterLogic extends FilterLogic {
	private final BackpackInventoryHandler inventoryHandler;

	public ContentsFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, BackpackInventoryHandler inventoryHandler) {
		super(upgrade, saveHandler, filterSlotCount);
		this.inventoryHandler = inventoryHandler;
	}

	public ContentsFilterType getFilterType() {
		if (shouldFilterByBackpack()) {
			return ContentsFilterType.BACKPACK;
		}
		return isAllowList() ? ContentsFilterType.ALLOW : ContentsFilterType.BLOCK;
	}

	public void setDepositFilterType(ContentsFilterType contentsFilterType) {
		switch (contentsFilterType) {
			case ALLOW -> {
				setFilterByBackpack(false);
				setAllowList(true);
			}
			case BLOCK -> {
				setFilterByBackpack(false);
				setAllowList(false);
			}
			case BACKPACK -> {
				setFilterByBackpack(true);
				save();
			}
		}
	}

	@Override
	public boolean matchesFilter(ItemStack stack) {
		if (!shouldFilterByBackpack()) {
			return super.matchesFilter(stack);
		}

		for (ItemStackKey filterStack : inventoryHandler.getSlotTracker().getFullStacks()) {
			if (stackMatchesFilter(stack, filterStack.getStack())) {
				return true;
			}
		}
		for (ItemStackKey filterStack : inventoryHandler.getSlotTracker().getPartialStacks()) {
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
