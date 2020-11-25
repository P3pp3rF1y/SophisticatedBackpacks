package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class InventoryHelper {
	private InventoryHelper() {}

	public static void copyTo(IItemHandlerModifiable handler, IItemHandlerModifiable otherHandler) {
		for (int slot = 0; slot < handler.getSlots() && slot < otherHandler.getSlots(); slot++) {
			ItemStack slotStack = handler.getStackInSlot(slot);
			if (!slotStack.isEmpty()) {
				otherHandler.setStackInSlot(slot, slotStack);
			}
		}
	}

	public static ItemStack insertIntoInventory(ItemStack stack, IItemHandler inventory, boolean simulate) {
		ItemStack remainingStack = stack.copy();
		for (int slot = 0; slot < inventory.getSlots() && !remainingStack.isEmpty(); slot++) {
			remainingStack = inventory.insertItem(slot, remainingStack, simulate);
		}
		return remainingStack;
	}

	public static ItemStack extractFromInventory(Item item, int count, IItemHandler inventory, boolean simulate) {
		ItemStack ret = ItemStack.EMPTY;
		for (int slot = 0; slot < inventory.getSlots() && ret.getCount() < count; slot++) {
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
		for (int slot = 0; slot < inventory.getSlots() && extractedCount < stack.getCount(); slot++) {
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
		for (int slot = 0; slot < handler.getSlots(); slot++) {
			ItemStack stack = handler.getStackInSlot(slot);
			actOn.accept(slot, stack);
		}
	}

	public static <T> T iterate(IItemHandler handler, BiFunction<Integer, ItemStack, T> getFromSlotStack, Supplier<T> supplyDefault, Predicate<T> shouldExit) {
		T ret = supplyDefault.get();
		for (int slot = 0; slot < handler.getSlots(); slot++) {
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
		for (int slot = 0; slot < handlerA.getSlots(); slot++) {
			ItemStack slotStack = handlerA.getStackInSlot(slot);
			ItemStack resultStack = insertIntoInventory(slotStack, handlerB, true);
			int countToExtract = slotStack.getCount() - resultStack.getCount();
			if (countToExtract > 0 && handlerA.extractItem(slot, countToExtract, true).getCount() == countToExtract) {
				InventoryHelper.insertIntoInventory(handlerA.extractItem(slot, countToExtract, false), handlerB, false);
			}
		}
	}
}
