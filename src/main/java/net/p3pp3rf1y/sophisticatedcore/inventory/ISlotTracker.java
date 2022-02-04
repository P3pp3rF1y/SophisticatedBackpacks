package net.p3pp3rf1y.sophisticatedcore.inventory;

import net.minecraft.world.item.ItemStack;

import java.util.Collections;
import java.util.Set;
import java.util.function.UnaryOperator;

public interface ISlotTracker {

	Set<ItemStackKey> getFullStacks();

	Set<ItemStackKey> getPartialStacks();

	void removeAndSetSlotIndexes(InventoryHandler inventoryHandler, int slot, ItemStack stack);

	void clear();

	void refreshSlotIndexesFrom(InventoryHandler itemHandler);

	ItemStack insertItemIntoHandler(InventoryHandler itemHandler, IItemHandlerInserter inserter, UnaryOperator<ItemStack> overflowHandler, ItemStack stack, boolean simulate);

	ItemStack insertItemIntoHandler(InventoryHandler itemHandler, IItemHandlerInserter inserter, UnaryOperator<ItemStack> overflowHandler, int slot, ItemStack stack, boolean simulate);

	interface IItemHandlerInserter {
		ItemStack insertItem(int slot, ItemStack stack, boolean simulate);
	}

	class Noop implements ISlotTracker {
		@Override
		public Set<ItemStackKey> getFullStacks() {
			return Collections.emptySet();
		}

		@Override
		public Set<ItemStackKey> getPartialStacks() {
			return Collections.emptySet();
		}

		@Override
		public void removeAndSetSlotIndexes(InventoryHandler inventoryHandler, int slot, ItemStack stack) {
			//noop
		}

		@Override
		public void clear() {
			//noop
		}

		@Override
		public void refreshSlotIndexesFrom(InventoryHandler itemHandler) {
			//noop
		}

		@Override
		public ItemStack insertItemIntoHandler(InventoryHandler itemHandler, IItemHandlerInserter inserter, UnaryOperator<ItemStack> overflowHandler, ItemStack stack, boolean simulate) {
			ItemStack remainingStack = stack.copy();
			int slots = itemHandler.getSlots();
			for (int slot = 0; slot < slots && !remainingStack.isEmpty(); slot++) {
				remainingStack = inserter.insertItem(slot, remainingStack, simulate);
			}

			return remainingStack;
		}

		@Override
		public ItemStack insertItemIntoHandler(InventoryHandler itemHandler, IItemHandlerInserter inserter, UnaryOperator<ItemStack> overflowHandler, int slot, ItemStack stack, boolean simulate) {
			return inserter.insertItem(slot, stack, simulate);
		}
	}
}
