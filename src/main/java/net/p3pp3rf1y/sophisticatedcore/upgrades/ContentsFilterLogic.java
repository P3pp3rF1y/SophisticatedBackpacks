package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class ContentsFilterLogic extends FilterLogic {

	private final Supplier<InventoryHandler> getInventoryHandler;
	private final MemorySettingsCategory memorySettings;

	public ContentsFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, Supplier<InventoryHandler> getInventoryHandler, MemorySettingsCategory memorySettings) {
		super(upgrade, saveHandler, filterSlotCount);
		this.getInventoryHandler = getInventoryHandler;
		this.memorySettings = memorySettings;
	}

	public ContentsFilterType getFilterType() {
		if (shouldFilterByStorage()) {
			return ContentsFilterType.STORAGE;
		}
		return isAllowList() ? ContentsFilterType.ALLOW : ContentsFilterType.BLOCK;
	}

	public void setDepositFilterType(ContentsFilterType contentsFilterType) {
		switch (contentsFilterType) {
			case ALLOW -> {
				setFilterByStorage(false);
				setAllowList(true);
			}
			case BLOCK -> {
				setFilterByStorage(false);
				setAllowList(false);
			}
			case STORAGE -> {
				setFilterByStorage(true);
				save();
			}
		}
	}

	@Override
	public boolean matchesFilter(ItemStack stack) {
		if (!shouldFilterByStorage()) {
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

	private void setFilterByStorage(boolean filterByStorage) {
		NBTHelper.setBoolean(upgrade, "filterByStorage", filterByStorage);
		save();
	}

	private boolean shouldFilterByStorage() {
		return NBTHelper.getBoolean(upgrade, "filterByStorage").orElse(false);
	}
}
