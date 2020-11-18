package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
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

	public static boolean runPickupOnBackpack(World world, ItemStack remainingStack, IBackpackWrapper backpackWrapper, boolean simulate) {
		List<IPickupResponseUpgrade> pickupUpgrades = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IPickupResponseUpgrade.class);

		for (IPickupResponseUpgrade pickupUpgrade : pickupUpgrades) {
			if (pickupUpgrade.getCooldownTime() <= world.getGameTime()) {
				ItemStack ret = pickupUpgrade.pickup(world, remainingStack, backpackWrapper, simulate);
				remainingStack.setCount(ret.getCount());
				if (remainingStack.isEmpty()) {
					return true;
				}
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
}
