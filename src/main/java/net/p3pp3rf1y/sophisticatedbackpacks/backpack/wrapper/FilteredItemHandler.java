package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;

import java.util.List;

public class FilteredItemHandler<T extends IItemHandler> implements IItemHandler {
	protected final T inventoryHandler;
	private final List<FilterLogic> inputFilters;
	private final List<FilterLogic> outputFilters;

	public FilteredItemHandler(T inventoryHandler, List<FilterLogic> inputFilters, List<FilterLogic> outputFilters) {
		this.inventoryHandler = inventoryHandler;
		this.inputFilters = inputFilters;
		this.outputFilters = outputFilters;
	}

	@Override
	public int getSlots() {
		return inventoryHandler.getSlots();
	}

	@Override
	public ItemStack getStackInSlot(int slot) {
		return inventoryHandler.getStackInSlot(slot);
	}

	@Override
	public ItemStack insertItem(int slot, ItemStack stack, boolean simulate) {
		if (inputFilters.isEmpty()) {
			return inventoryHandler.insertItem(slot, stack, simulate);
		}

		for (FilterLogic filter : inputFilters) {
			if (filter.matchesFilter(stack)) {
				return inventoryHandler.insertItem(slot, stack, simulate);
			}
		}
		return stack;
	}

	@Override
	public ItemStack extractItem(int slot, int amount, boolean simulate) {
		if (outputFilters.isEmpty()) {
			return inventoryHandler.extractItem(slot, amount, simulate);
		}

		for (FilterLogic filter : outputFilters) {
			if (filter.matchesFilter(getStackInSlot(slot))) {
				return inventoryHandler.extractItem(slot, amount, simulate);
			}
		}
		return ItemStack.EMPTY;
	}

	@Override
	public int getSlotLimit(int slot) {
		return inventoryHandler.getSlotLimit(slot);
	}

	@Override
	public boolean isItemValid(int slot, ItemStack stack) {
		return inventoryHandler.isItemValid(slot, stack);
	}

	public static class Modifiable extends FilteredItemHandler<IItemHandlerModifiable> implements IItemHandlerModifiable {
		public Modifiable(IItemHandlerModifiable inventoryHandler, List<FilterLogic> inputFilters, List<FilterLogic> outputFilters) {
			super(inventoryHandler, inputFilters, outputFilters);
		}

		@Override
		public void setStackInSlot(int slot, ItemStack stack) {
			inventoryHandler.setStackInSlot(slot, stack);
		}
	}
}
