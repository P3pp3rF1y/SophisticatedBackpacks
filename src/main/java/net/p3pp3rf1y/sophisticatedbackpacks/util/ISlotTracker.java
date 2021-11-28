package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;

import java.util.Collections;
import java.util.Set;

public interface ISlotTracker {

	Set<ItemStackKey> getFullStacks();

	Set<ItemStackKey> getPartialStacks();

	void removeAndSetSlotIndexes(BackpackInventoryHandler inventoryHandler, int slot, ItemStack stack);

	void clear();

	void refreshSlotIndexesFrom(BackpackInventoryHandler itemHandler);

	ItemStack insertItemIntoHandler(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, ItemStack stack, boolean simulate);

	ItemStack insertItemIntoHandler(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, int slot, ItemStack stack, boolean simulate);

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
		public void removeAndSetSlotIndexes(BackpackInventoryHandler inventoryHandler, int slot, ItemStack stack) {
			//noop
		}

		@Override
		public void clear() {
			//noop
		}

		@Override
		public void refreshSlotIndexesFrom(BackpackInventoryHandler itemHandler) {
			//noop
		}

		@Override
		public ItemStack insertItemIntoHandler(BackpackInventoryHandler itemHandler, InventoryHandlerSlotTracker.IItemHandlerInserter inserter, ItemStack stack, boolean simulate) {
			ItemStack remainingStack = stack.copy();
			int slots = itemHandler.getSlots();
			for (int slot = 0; slot < slots && !remainingStack.isEmpty(); slot++) {
				remainingStack = inserter.insertItem(slot, remainingStack, simulate);
			}

			return remainingStack;
		}

		@Override
		public ItemStack insertItemIntoHandler(BackpackInventoryHandler itemHandler, InventoryHandlerSlotTracker.IItemHandlerInserter inserter, int slot, ItemStack stack, boolean simulate) {
			return inserter.insertItem(slot, stack, simulate);
		}
	}
}
