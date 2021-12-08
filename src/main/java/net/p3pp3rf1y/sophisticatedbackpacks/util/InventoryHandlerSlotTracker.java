package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsCategory;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class InventoryHandlerSlotTracker implements ISlotTracker {
	private final Map<ItemStackKey, Set<Integer>> fullStackSlots = new HashMap<>();
	private final Map<Integer, ItemStackKey> fullSlotStacks = new HashMap<>();
	private final Map<ItemStackKey, Set<Integer>> partiallyFilledStackSlots = new HashMap<>();
	private final Map<Integer, ItemStackKey> partiallyFilledSlotStacks = new HashMap<>();
	private final Set<Integer> emptySlots = new TreeSet<>();
	private final MemorySettingsCategory memorySettings;

	public InventoryHandlerSlotTracker(MemorySettingsCategory memorySettings) {
		this.memorySettings = memorySettings;
	}

	public void addPartiallyFilled(int slot, ItemStack stack) {
		ItemStackKey stackKey = new ItemStackKey(stack);
		partiallyFilledStackSlots.computeIfAbsent(stackKey, k -> new TreeSet<>()).add(slot);
		partiallyFilledSlotStacks.put(slot, stackKey);
	}

	@Override
	public Set<ItemStackKey> getFullStacks() {
		return fullStackSlots.keySet();
	}

	@Override
	public Set<ItemStackKey> getPartialStacks() {
		return partiallyFilledStackSlots.keySet();
	}

	public void addFull(int slot, ItemStack stack) {
		ItemStackKey stackKey = new ItemStackKey(stack);
		fullStackSlots.computeIfAbsent(stackKey, k -> new HashSet<>()).add(slot);
		fullSlotStacks.put(slot, stackKey);
	}

	public void removePartiallyFilled(int slot) {
		if (partiallyFilledSlotStacks.containsKey(slot)) {
			ItemStackKey stackKey = partiallyFilledSlotStacks.remove(slot);
			Set<Integer> partialSlots = partiallyFilledStackSlots.get(stackKey);
			partialSlots.remove(slot);
			if (partialSlots.isEmpty()) {
				partiallyFilledStackSlots.remove(stackKey);
			}
		}
	}

	private void removeFull(int slot) {
		if (fullSlotStacks.containsKey(slot)) {
			ItemStackKey stackKey = fullSlotStacks.remove(slot);
			Set<Integer> fullSlots = fullStackSlots.get(stackKey);
			fullSlots.remove(slot);
			if (fullSlots.isEmpty()) {
				fullStackSlots.remove(stackKey);
			}
		}
	}

	@Override
	public void removeAndSetSlotIndexes(BackpackInventoryHandler inventoryHandler, int slot, ItemStack stack) {
		removePartiallyFilled(slot);
		removeFull(slot);
		emptySlots.remove(slot);

		set(inventoryHandler, slot, stack);
	}

	private void set(BackpackInventoryHandler inventoryHandler, int slot, ItemStack stack) {
		if (stack.isEmpty()) {
			emptySlots.add(slot);
		} else {
			if (isPartiallyFilled(inventoryHandler, slot, stack)) {
				addPartiallyFilled(slot, stack);
			} else {
				addFull(slot, stack);
			}
		}
	}

	@Override
	public void clear() {
		partiallyFilledStackSlots.clear();
		partiallyFilledSlotStacks.clear();
	}

	@Override
	public void refreshSlotIndexesFrom(BackpackInventoryHandler itemHandler) {
		fullStackSlots.clear();
		fullSlotStacks.clear();
		partiallyFilledSlotStacks.clear();
		partiallyFilledStackSlots.clear();
		emptySlots.clear();

		for (int slot = 0; slot < itemHandler.getSlots(); slot++) {
			ItemStack stack = itemHandler.getStackInSlot(slot);
			set(itemHandler, slot, stack);
		}
	}

	private boolean isPartiallyFilled(BackpackInventoryHandler itemHandler, int slot, ItemStack stack) {
		return stack.getCount() < itemHandler.getStackLimit(slot, stack);
	}

	@Override
	public ItemStack insertItemIntoHandler(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, ItemStack stack, boolean simulate) {
		ItemStack remainingStack = insertIntoSlotsThatMatchStack(inserter, stack, simulate);
		if (!remainingStack.isEmpty()) {
			remainingStack = insertIntoEmptySlots(inserter, remainingStack, simulate);
		}
		return remainingStack;
	}

	@Override
	public ItemStack insertItemIntoHandler(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, int slot, ItemStack stack, boolean simulate) {
		ItemStack existing = itemHandler.getStackInSlot(slot);
		boolean wasEmpty = existing.isEmpty();

		ItemStack remainingStack = stack;
		boolean doesNotMatchCurrentSlot = !ItemHandlerHelper.canItemStacksStack(stack, existing);
		if (wasEmpty || doesNotMatchCurrentSlot) {
			remainingStack = insertIntoSlotsThatMatchStack(inserter, remainingStack, simulate);
		}
		if (!remainingStack.isEmpty() && doesNotMatchCurrentSlot) {
			remainingStack = insertIntoEmptySlots(inserter, remainingStack, simulate);
		}
		if (!remainingStack.isEmpty()) {
			remainingStack = inserter.insertItem(slot, remainingStack, simulate);
		}

		return remainingStack;
	}

	private ItemStack insertIntoSlotsThatMatchStack(IItemHandlerInserter inserter, ItemStack stack, boolean simulate) {
		ItemStack remainingStack = stack;
		ItemStackKey stackKey = new ItemStackKey(stack);

		int sizeBefore = partiallyFilledStackSlots.containsKey(stackKey) ? partiallyFilledStackSlots.get(stackKey).size() : 0;
		int i = 0;
		// Always taking first element here and iterating while not empty as iterating using iterator would produce CME due to void/compacting reacting to inserts
		// and going into this logic as well and because of that causing collection to be updated outside of first level iterator. The increment is here just
		// in case updating cache fails to prevent infinite loop
		while (partiallyFilledStackSlots.get(stackKey) != null && !partiallyFilledStackSlots.get(stackKey).isEmpty() && i++ < sizeBefore) {
			int matchingSlot = partiallyFilledStackSlots.get(stackKey).iterator().next();
			remainingStack = inserter.insertItem(matchingSlot, remainingStack, simulate);
			if (remainingStack.isEmpty()) {
				break;
			}
		}
		return remainingStack;
	}

	private ItemStack insertIntoEmptySlots(IItemHandlerInserter inserter, ItemStack stack, boolean simulate) {
		ItemStack remainingStack = stack.copy();
		remainingStack = insertIntoEmptyMemorySlots(inserter, simulate, remainingStack);
		if (!remainingStack.isEmpty()) {
			int sizeBefore = emptySlots.size();
			int i = 0;
			// Always taking first element here and iterating while not empty as iterating using iterator would produce CME due to void/compacting reacting to inserts
			// and going into this logic as well and because of that causing collection to be updated outside of first level iterator. The increment is here just
			// in case updating cache fails to prevent infinite loop
			while (!emptySlots.isEmpty() && i++ < sizeBefore) {
				int slot = emptySlots.iterator().next();
				remainingStack = inserter.insertItem(slot, remainingStack, simulate);
				if (remainingStack.isEmpty()) {
					break;
				}
			}
		}

		return remainingStack;
	}

	private ItemStack insertIntoEmptyMemorySlots(IItemHandlerInserter inserter, boolean simulate, ItemStack stack) {
		ItemStack remainingStack = stack;
		Map<Item, Set<Integer>> memoryFilterItemSlots = memorySettings.getFilterItemSlots();
		Item item = remainingStack.getItem();
		if (memoryFilterItemSlots.containsKey(item)) {
			for (int memorySlot : memoryFilterItemSlots.get(item)) {
				if (emptySlots.contains(memorySlot)) {
					remainingStack = inserter.insertItem(memorySlot, remainingStack, simulate);
					if (remainingStack.isEmpty()) {
						break;
					}
				}
			}
		}
		return remainingStack;
	}
}
