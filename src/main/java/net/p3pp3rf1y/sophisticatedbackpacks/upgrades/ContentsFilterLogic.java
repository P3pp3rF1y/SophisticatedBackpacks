package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemStackKey;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class ContentsFilterLogic extends FilterLogic {

	private final Supplier<BackpackInventoryHandler> getInventoryHandler;
	private final MemorySettingsCategory memorySettings;

	public ContentsFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, Supplier<BackpackInventoryHandler> getInventoryHandler, MemorySettingsCategory memorySettings) {
		super(upgrade, saveHandler, filterSlotCount);
		this.getInventoryHandler = getInventoryHandler;
		this.memorySettings = memorySettings;
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

		for (ItemStackKey filterStack : getInventoryHandler.get().getSlotTracker().getFullStacks()) {
			if (stackMatchesFilter(stack, filterStack.getStack())) {
				return true;
			}
		}
		for (ItemStackKey filterStack : getInventoryHandler.get().getSlotTracker().getPartialStacks()) {
			if (stackMatchesFilter(stack, filterStack.getStack())) {
				return true;
			}
		}
		return memorySettings.getFilterItemSlots().containsKey(stack.getItem());
	}

	private void setFilterByBackpack(boolean filterByBackpack) {
		NBTHelper.setBoolean(upgrade, "filterByBackpack", filterByBackpack);
		save();
	}

	private boolean shouldFilterByBackpack() {
		return NBTHelper.getBoolean(upgrade, "filterByBackpack").orElse(false);
	}
}
