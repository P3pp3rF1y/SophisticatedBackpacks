package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.ItemHandlerHelper;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class InventorySorter {
	private InventorySorter() {}

	public static final Comparator<Map.Entry<FilterStack, Integer>> BY_NAME = Comparator.comparing(o -> getRegistryName(o.getKey()));

	public static final Comparator<Map.Entry<FilterStack, Integer>> BY_COUNT = (first, second) -> {
		int ret = second.getValue().compareTo(first.getValue());
		return ret != 0 ? ret : getRegistryName(first.getKey()).compareTo(getRegistryName(second.getKey()));
	};

	public static final Comparator<Map.Entry<FilterStack, Integer>> BY_TAGS = new Comparator<Map.Entry<FilterStack, Integer>>() {
		@Override
		public int compare(Map.Entry<FilterStack, Integer> first, Map.Entry<FilterStack, Integer> second) {
			Item firstItem = first.getKey().stack.getItem();
			Item secondItem = second.getKey().stack.getItem();
			if (firstItem == secondItem) {
				return 0;
			}
			int ret = compareTags(firstItem.getTags(), secondItem.getTags());
			return ret != 0 ? ret : getRegistryName(first.getKey()).compareTo(getRegistryName(second.getKey()));
		}

		private int compareTags(Set<ResourceLocation> firstTags, Set<ResourceLocation> secondTags) {
			int ret = Integer.compare(secondTags.size(), firstTags.size());
			if (ret != 0) {
				return ret;
			}

			if (firstTags.size() == 1) {
				return firstTags.iterator().next().compareTo(secondTags.iterator().next());
			}

			ArrayList<ResourceLocation> firstTagsSorted = new ArrayList<>(firstTags);
			ArrayList<ResourceLocation> secondTagsSorted = new ArrayList<>(secondTags);
			firstTagsSorted.sort(Comparator.naturalOrder());
			secondTagsSorted.sort(Comparator.naturalOrder());

			for (int i = 0; i < firstTagsSorted.size(); i++) {
				ret = firstTagsSorted.get(i).compareTo(secondTagsSorted.get(i));
				if (ret != 0) {
					return ret;
				}
			}
			return 0;
		}
	};

	private static String getRegistryName(FilterStack filterStack) {
		//noinspection ConstantConditions - registryName is nonNull by the time it exists in itemstack form
		return filterStack.stack.getItem().getRegistryName().toString();
	}

	public static void sortHandler(IItemHandlerModifiable handler, Comparator<? super Map.Entry<FilterStack, Integer>> comparator) {
		Map<FilterStack, Integer> compactedStacks = getCompactedStacks(handler);
		List<Map.Entry<FilterStack, Integer>> sortedList = new ArrayList<>(compactedStacks.entrySet());
		sortedList.sort(comparator);

		int slots = handler.getSlots();
		Iterator<Map.Entry<FilterStack, Integer>> it = sortedList.iterator();
		FilterStack current = null;
		int count = 0;
		for (int slot = 0; slot < slots; slot++) {
			if ((current == null || count <= 0) && it.hasNext()) {
				Map.Entry<FilterStack, Integer> entry = it.next();
				current = entry.getKey();
				count = entry.getValue();
			}
			if (current != null && count > 0) {
				ItemStack copy = current.stack.copy();
				int countToPlace = Math.min(count, copy.getMaxStackSize());
				copy.setCount(countToPlace);
				if (!ItemStack.areItemStacksEqual(handler.getStackInSlot(slot), copy)) {
					handler.setStackInSlot(slot, copy);
				}
				count -= countToPlace;
			} else {
				if (!handler.getStackInSlot(slot).isEmpty()) {
					handler.setStackInSlot(slot, ItemStack.EMPTY);
				}
			}
		}
	}

	public static Map<FilterStack, Integer> getCompactedStacks(IItemHandler handler) {
		Map<FilterStack, Integer> ret = new HashMap<>();
		InventoryHelper.iterate(handler, (slot, stack) -> {
			if (stack.isEmpty()) {
				return;
			}
			FilterStack filterStack = new FilterStack(stack);
			ret.put(filterStack, ret.computeIfAbsent(filterStack, fs -> 0) + stack.getCount());
		});
		return ret;
	}

	public static class FilterStack {
		private final ItemStack stack;

		public FilterStack(ItemStack stack) {
			this.stack = stack.copy();
		}

		@Override
		public boolean equals(Object o) {
			if (this == o) { return true; }
			if (o == null || getClass() != o.getClass()) { return false; }
			FilterStack that = (FilterStack) o;
			return ItemHandlerHelper.canItemStacksStack(stack, that.stack);
		}

		@Override
		public int hashCode() {
			//noinspection ConstantConditions - hasTag call makes sure getTag doesn't return null
			return stack.getItem().hashCode() * 31 + (stack.hasTag() ? stack.getTag().hashCode() : 0);
		}
	}
}
