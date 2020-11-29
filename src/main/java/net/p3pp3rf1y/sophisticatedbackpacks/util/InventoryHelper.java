package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import org.apache.commons.lang3.mutable.MutableInt;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BooleanSupplier;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class InventoryHelper {
	private InventoryHelper() {}

	public static void copyTo(IItemHandlerModifiable handlerA, IItemHandlerModifiable handlerB) {
		int slotsA = handlerA.getSlots();
		int slotsB = handlerB.getSlots();
		for (int slot = 0; slot < slotsA && slot < slotsB; slot++) {
			ItemStack slotStack = handlerA.getStackInSlot(slot);
			if (!slotStack.isEmpty()) {
				handlerB.setStackInSlot(slot, slotStack);
			}
		}
	}

	public static ItemStack insertIntoInventory(ItemStack stack, IItemHandler inventory, boolean simulate) {
		ItemStack remainingStack = stack.copy();
		int slots = inventory.getSlots();
		for (int slot = 0; slot < slots && !remainingStack.isEmpty(); slot++) {
			remainingStack = inventory.insertItem(slot, remainingStack, simulate);
		}
		return remainingStack;
	}

	public static ItemStack extractFromInventory(Item item, int count, IItemHandler inventory, boolean simulate) {
		ItemStack ret = ItemStack.EMPTY;
		int slots = inventory.getSlots();
		for (int slot = 0; slot < slots && ret.getCount() < count; slot++) {
			ItemStack slotStack = inventory.getStackInSlot(slot);
			if (slotStack.getItem() == item && (ret.isEmpty() || ItemHandlerHelper.canItemStacksStack(ret, slotStack))) {
				int toExtract = Math.min(slotStack.getCount(), count - ret.getCount());
				ItemStack extractedStack = inventory.extractItem(slot, toExtract, simulate);
				if (ret.isEmpty()) {
					ret = extractedStack;
				} else {
					ret.setCount(ret.getCount() + extractedStack.getCount());
				}
			}
		}
		return ret;
	}

	public static ItemStack extractFromInventory(ItemStack stack, IItemHandler inventory, boolean simulate) {
		int extractedCount = 0;
		int slots = inventory.getSlots();
		for (int slot = 0; slot < slots && extractedCount < stack.getCount(); slot++) {
			ItemStack slotStack = inventory.getStackInSlot(slot);
			if (ItemHandlerHelper.canItemStacksStack(stack, slotStack)) {
				int toExtract = Math.min(slotStack.getCount(), stack.getCount() - extractedCount);
				extractedCount += inventory.extractItem(slot, toExtract, simulate).getCount();
			}
		}

		if (extractedCount == 0) {
			return ItemStack.EMPTY;
		}

		ItemStack result = stack.copy();
		result.setCount(extractedCount);

		return result;
	}

	public static boolean runPickupOnBackpack(World world, ItemStack remainingStack, IBackpackWrapper backpackWrapper, boolean simulate) {
		List<IPickupResponseUpgrade> pickupUpgrades = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IPickupResponseUpgrade.class);

		for (IPickupResponseUpgrade pickupUpgrade : pickupUpgrades) {
			ItemStack ret = pickupUpgrade.pickup(world, remainingStack, backpackWrapper, simulate);
			remainingStack.setCount(ret.getCount());
			if (remainingStack.isEmpty()) {
				return true;
			}
		}

		return false;
	}

	public static void iterate(IItemHandler handler, BiConsumer<Integer, ItemStack> actOn) {
		iterate(handler, actOn, () -> false);
	}

	public static void iterate(IItemHandler handler, BiConsumer<Integer, ItemStack> actOn, BooleanSupplier shouldExit) {
		int slots = handler.getSlots();
		for (int slot = 0; slot < slots; slot++) {
			ItemStack stack = handler.getStackInSlot(slot);
			actOn.accept(slot, stack);
			if (shouldExit.getAsBoolean()) {
				break;
			}
		}
	}

	public static int getCountMissingInHandler(IItemHandler itemHandler, ItemStack filter, int expectedCount) {
		MutableInt missingCount = new MutableInt(expectedCount);
		iterate(itemHandler, (slot, stack) -> {
			if (ItemHandlerHelper.canItemStacksStack(stack, filter)) {
				missingCount.subtract(Math.min(stack.getCount(), missingCount.getValue()));
			}
		}, () -> missingCount.getValue() == 0);
		return missingCount.getValue();
	}

	public static <T> T iterate(IItemHandler handler, BiFunction<Integer, ItemStack, T> getFromSlotStack, Supplier<T> supplyDefault, Predicate<T> shouldExit) {
		T ret = supplyDefault.get();
		int slots = handler.getSlots();
		for (int slot = 0; slot < slots; slot++) {
			ItemStack stack = handler.getStackInSlot(slot);
			ret = getFromSlotStack.apply(slot, stack);
			if (shouldExit.test(ret)) {
				break;
			}
		}
		return ret;
	}

	public static boolean anyStackTagMatches(ItemStack stackA, ItemStack stackB) {
		return anyItemTagMatches(stackA.getItem(), stackB.getItem());
	}

	public static boolean anyItemTagMatches(Item itemA, Item itemB) {
		for (ResourceLocation tag : itemA.getTags()) {
			if (itemB.getTags().contains(tag)) {
				return true;
			}
		}
		return false;
	}

	public static void transfer(IItemHandler handlerA, IItemHandler handlerB) {
		int slotsA = handlerA.getSlots();
		for (int slot = 0; slot < slotsA; slot++) {
			ItemStack slotStack = handlerA.getStackInSlot(slot);
			if (slotStack.isEmpty()) {
				continue;
			}

			ItemStack resultStack = insertIntoInventory(slotStack, handlerB, true);
			int countToExtract = slotStack.getCount() - resultStack.getCount();
			if (countToExtract > 0 && handlerA.extractItem(slot, countToExtract, true).getCount() == countToExtract) {
				InventoryHelper.insertIntoInventory(handlerA.extractItem(slot, countToExtract, false), handlerB, false);
			}
		}
	}

	public static void moveBetweenInventories(IItemHandler extractFromHandler, IItemHandler insertIntoHandler, ItemStack filter, int count) {
		ItemStack toMove = filter.copy();
		toMove.setCount(count);
		ItemStack extracted = extractFromInventory(toMove, extractFromHandler, true);
		if (extracted.isEmpty()) {
			return;
		}
		ItemStack remaining = insertIntoInventory(extracted, insertIntoHandler, true);
		if (remaining.getCount() == extracted.getCount()) {
			return;
		}
		toMove.setCount(extracted.getCount() - remaining.getCount());
		insertIntoInventory(extractFromInventory(toMove, extractFromHandler, false), insertIntoHandler, false);
	}
}
