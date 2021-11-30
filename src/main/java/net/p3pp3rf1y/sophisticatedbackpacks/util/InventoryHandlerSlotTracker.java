package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsCategory;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
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

	private void addFull(int slot, ItemStack stack) {
		ItemStackKey stackKey = new ItemStackKey(stack);
		fullStackSlots.computeIfAbsent(stackKey, k -> new HashSet<>()).add(slot);
		fullSlotStacks.put(slot, stackKey);
	}

	private void removePartiallyFilled(int slot, Iterator<Integer> iterator) {
		if (partiallyFilledSlotStacks.containsKey(slot)) {
			iterator.remove();
			ItemStackKey stackKey = partiallyFilledSlotStacks.remove(slot);
			if (partiallyFilledStackSlots.get(stackKey).isEmpty()) {
				partiallyFilledStackSlots.remove(stackKey);
			}
		}
	}

	private void removePartiallyFilled(int slot) {
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

	private Set<Integer> getPartiallyFilledStackSlots(ItemStack stack) {
		ItemStackKey stackKey = new ItemStackKey(stack);
		return partiallyFilledStackSlots.getOrDefault(stackKey, Collections.emptySet());
	}

	private Set<Integer> getEmptySlots() {
		return emptySlots;
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
		ItemStack remainingStack = insertIntoSlotsThatMatchStack(itemHandler, inserter, stack, simulate);
		if (!remainingStack.isEmpty()) {
			remainingStack = insertIntoEmptySlots(itemHandler, inserter, remainingStack, simulate);
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
			remainingStack = insertIntoSlotsThatMatchStack(itemHandler, inserter, remainingStack, simulate);
		}
		if (!remainingStack.isEmpty() && doesNotMatchCurrentSlot) {
			remainingStack = insertIntoEmptySlots(itemHandler, inserter, remainingStack, simulate);
		}
		if (!remainingStack.isEmpty()) {
			int countBeforeInsert = remainingStack.getCount();
			remainingStack = inserter.insertItem(slot, remainingStack, simulate);
			if (!simulate && doesNotMatchCurrentSlot && countBeforeInsert > remainingStack.getCount()) {
				set(itemHandler, slot, stack);
			}
		}

		return remainingStack;
	}

	private ItemStack insertIntoSlotsThatMatchStack(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, ItemStack stack, boolean simulate) {
		ItemStack remainingStack = stack;
		for (Iterator<Integer> iterator = getPartiallyFilledStackSlots(stack).iterator(); iterator.hasNext(); ) {
			int matchingSlot = iterator.next();
			int countBeforeInsert = remainingStack.getCount();
			remainingStack = inserter.insertItem(matchingSlot, remainingStack, simulate);
			if (countBeforeInsert > remainingStack.getCount()) {
				if (!simulate) {
					ItemStack stackAfterInsert = itemHandler.getStackInSlot(matchingSlot);
					if (stackAfterInsert.getCount() == itemHandler.getStackLimit(matchingSlot, stack)) {
						removePartiallyFilled(matchingSlot, iterator);
						addFull(matchingSlot, stackAfterInsert);
					}
				}
				if (remainingStack.isEmpty()) {
					break;
				}
			}
		}
		return remainingStack;
	}

	private ItemStack insertIntoEmptySlots(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, ItemStack stack, boolean simulate) {
		ItemStack remainingStack = stack.copy();
		remainingStack = insertIntoEmptyMemorySlots(itemHandler, inserter, simulate, remainingStack);
		if (!remainingStack.isEmpty()) {
			for (Iterator<Integer> iterator = getEmptySlots().iterator(); iterator.hasNext(); ) {
				int slot = iterator.next();
				remainingStack = insertStackInSlot(itemHandler, inserter, stack, simulate, remainingStack, slot, iterator::remove);
				if (remainingStack.isEmpty()) {
					break;
				}
			}
		}

		return remainingStack;
	}

	private ItemStack insertIntoEmptyMemorySlots(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, boolean simulate, ItemStack stack) {
		ItemStack remainingStack = stack;
		Map<Item, Set<Integer>> memoryFilterItemSlots = memorySettings.getFilterItemSlots();
		Item item = remainingStack.getItem();
		if (memoryFilterItemSlots.containsKey(item)) {
			for (int memorySlot : memoryFilterItemSlots.get(item)) {
				if (getEmptySlots().contains(memorySlot)) {
					remainingStack = insertStackInSlot(itemHandler, inserter, stack, simulate, remainingStack, memorySlot, () -> removeEmpty(memorySlot));
					if (remainingStack.isEmpty()) {
						break;
					}
				}
			}
		}
		return remainingStack;
	}

	private ItemStack insertStackInSlot(BackpackInventoryHandler itemHandler, IItemHandlerInserter inserter, ItemStack stack, boolean simulate, ItemStack remainingStack, int slot, Runnable removeEmpty) {
		remainingStack = inserter.insertItem(slot, remainingStack, simulate);
		if (!simulate && !itemHandler.getStackInSlot(slot).isEmpty()) {
			removeEmpty.run();
			set(itemHandler, slot, stack);
		}
		return remainingStack;
	}

	private void removeEmpty(Integer slot) {
		emptySlots.remove(slot);
	}

}
