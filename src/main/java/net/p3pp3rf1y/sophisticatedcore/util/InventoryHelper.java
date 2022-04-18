package net.p3pp3rf1y.sophisticatedcore.util;

import com.google.common.collect.Lists;
import com.google.common.util.concurrent.AtomicDouble;
import net.minecraft.core.BlockPos;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.util.Mth;
import net.minecraft.world.Containers;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.ItemHandlerHelper;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import org.apache.commons.lang3.mutable.MutableInt;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class InventoryHelper {
	private InventoryHelper() {}

	public static boolean hasItem(IItemHandler inventory, Predicate<ItemStack> matches) {
		AtomicBoolean result = new AtomicBoolean(false);
		iterate(inventory, (slot, stack) -> {
			if (!stack.isEmpty() && matches.test(stack)) {
				result.set(true);
			}
		}, result::get);
		return result.get();
	}

	public static Set<Integer> getItemSlots(IItemHandler inventory, Predicate<ItemStack> matches) {
		Set<Integer> slots = new HashSet<>();
		iterate(inventory, (slot, stack) -> {
			if (!stack.isEmpty() && matches.test(stack)) {
				slots.add(slot);
			}
		});
		return slots;
	}

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

	public static List<ItemStack> insertIntoInventory(List<ItemStack> stacks, IItemHandler inventory, boolean simulate) {
		if (stacks.isEmpty()) {
			return stacks;
		}
		IItemHandler targetInventory = inventory;
		if (simulate) {
			targetInventory = cloneInventory(inventory);
		}

		List<ItemStack> remaining = new ArrayList<>();
		for (ItemStack stack : stacks) {
			ItemStack result = insertIntoInventory(stack, targetInventory, false);
			if (!result.isEmpty()) {
				remaining.add(result);
			}
		}
		return remaining;
	}

	public static IItemHandler cloneInventory(IItemHandler inventory) {
		IItemHandler cloned = new ItemStackHandler(inventory.getSlots());
		for (int slot = 0; slot < inventory.getSlots(); slot++) {
			cloned.insertItem(slot, inventory.getStackInSlot(slot).copy(), false);
		}
		return cloned;
	}

	public static ItemStack insertIntoInventory(ItemStack stack, IItemHandler inventory, boolean simulate) {
		if (inventory instanceof IItemHandlerSimpleInserter itemHandlerSimpleInserter) {
			return itemHandlerSimpleInserter.insertItem(stack, simulate);
		}

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

	public static ItemStack runPickupOnPickupResponseUpgrades(Level world, UpgradeHandler upgradeHandler, ItemStack remainingStack, boolean simulate) {
		return runPickupOnPickupResponseUpgrades(world, null, upgradeHandler, remainingStack, simulate);
	}

	public static ItemStack runPickupOnPickupResponseUpgrades(Level world,
			@Nullable Player player, UpgradeHandler upgradeHandler, ItemStack remainingStack, boolean simulate) {
		List<IPickupResponseUpgrade> pickupUpgrades = upgradeHandler.getWrappersThatImplement(IPickupResponseUpgrade.class);

		for (IPickupResponseUpgrade pickupUpgrade : pickupUpgrades) {
			int countBeforePickup = remainingStack.getCount();
			remainingStack = pickupUpgrade.pickup(world, remainingStack, simulate);
			if (!simulate && player != null && remainingStack.getCount() != countBeforePickup) {
				playPickupSound(world, player);
			}

			if (remainingStack.isEmpty()) {
				return ItemStack.EMPTY;
			}
		}

		return remainingStack;
	}

	@SuppressWarnings("squid:S1764") // this actually isn't a case of identical values being used as both side are random float value thus -1 to 1 as a result
	private static void playPickupSound(Level world, @Nonnull Player player) {
		world.playSound(null, player.getX(), player.getY(), player.getZ(), SoundEvents.ITEM_PICKUP, SoundSource.PLAYERS, 0.2F, (world.random.nextFloat() - world.random.nextFloat()) * 1.4F + 2.0F);
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

	public static void transfer(IItemHandler handlerA, IItemHandler handlerB, Consumer<Supplier<ItemStack>> onInserted) {
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
				onInserted.accept(() -> {
					ItemStack copiedStack = slotStack.copy();
					copiedStack.setCount(countToExtract);
					return copiedStack;
				});
			}
		}
	}

	public static boolean isEmpty(IItemHandler itemHandler) {
		int slots = itemHandler.getSlots();
		for (int slot = 0; slot < slots; slot++) {
			if (!itemHandler.getStackInSlot(slot).isEmpty()) {
				return false;
			}
		}
		return true;
	}

	public static ItemStack getAndRemove(IItemHandler itemHandler, int slot) {
		if (slot >= itemHandler.getSlots()) {
			return ItemStack.EMPTY;
		}
		return itemHandler.extractItem(slot, itemHandler.getStackInSlot(slot).getCount(), false);
	}

	public static void insertOrDropItem(Player player, ItemStack stack, IItemHandler... inventories) {
		ItemStack ret = stack;
		for (IItemHandler inventory : inventories) {
			ret = insertIntoInventory(ret, inventory, false);
			if (ret.isEmpty()) {
				return;
			}
		}
		if (!ret.isEmpty()) {
			player.drop(ret, true);
		}
	}

	static Map<ItemStackKey, Integer> getCompactedStacks(IItemHandler handler) {
		return getCompactedStacks(handler, new HashSet<>());
	}

	static Map<ItemStackKey, Integer> getCompactedStacks(IItemHandler handler, Set<Integer> ignoreSlots) {
		Map<ItemStackKey, Integer> ret = new HashMap<>();
		iterate(handler, (slot, stack) -> {
			if (stack.isEmpty() || ignoreSlots.contains(slot)) {
				return;
			}
			ItemStackKey itemStackKey = new ItemStackKey(stack);
			ret.put(itemStackKey, ret.computeIfAbsent(itemStackKey, fs -> 0) + stack.getCount());
		});
		return ret;
	}

	public static List<ItemStack> getCompactedStacksSortedByCount(IItemHandler handler) {
		Map<ItemStackKey, Integer> compactedStacks = getCompactedStacks(handler);
		List<Map.Entry<ItemStackKey, Integer>> sortedList = new ArrayList<>(compactedStacks.entrySet());
		sortedList.sort(InventorySorter.BY_COUNT);

		List<ItemStack> ret = new ArrayList<>();
		sortedList.forEach(e -> {
			ItemStack stackCopy = e.getKey().getStack().copy();
			stackCopy.setCount(e.getValue());
			ret.add(stackCopy);
		});
		return ret;
	}

	public static Set<ItemStackKey> getUniqueStacks(IItemHandler handler) {
		Set<ItemStackKey> uniqueStacks = new HashSet<>();
		iterate(handler, (slot, stack) -> {
			if (stack.isEmpty()) {
				return;
			}
			ItemStackKey itemStackKey = new ItemStackKey(stack);
			uniqueStacks.add(itemStackKey);
		});
		return uniqueStacks;
	}

	public static List<Integer> getEmptySlotsRandomized(IItemHandler inventory, Random rand) {
		List<Integer> list = Lists.newArrayList();

		for (int i = 0; i < inventory.getSlots(); ++i) {
			if (inventory.getStackInSlot(i).isEmpty()) {
				list.add(i);
			}
		}

		Collections.shuffle(list, rand);
		return list;
	}

	public static void shuffleItems(List<ItemStack> stacks, int emptySlotsCount, Random rand) {
		List<ItemStack> list = Lists.newArrayList();
		Iterator<ItemStack> iterator = stacks.iterator();

		while (iterator.hasNext()) {
			ItemStack itemstack = iterator.next();
			if (itemstack.isEmpty()) {
				iterator.remove();
			} else if (itemstack.getCount() > 1) {
				list.add(itemstack);
				iterator.remove();
			}
		}

		while (emptySlotsCount - stacks.size() - list.size() > 0 && !list.isEmpty()) {
			ItemStack itemstack2 = list.remove(Mth.nextInt(rand, 0, list.size() - 1));
			int i = Mth.nextInt(rand, 1, itemstack2.getCount() / 2);
			ItemStack itemstack1 = itemstack2.split(i);
			if (itemstack2.getCount() > 1 && rand.nextBoolean()) {
				list.add(itemstack2);
			} else {
				stacks.add(itemstack2);
			}

			if (itemstack1.getCount() > 1 && rand.nextBoolean()) {
				list.add(itemstack1);
			} else {
				stacks.add(itemstack1);
			}
		}

		stacks.addAll(list);
		Collections.shuffle(stacks, rand);
	}

	public static void dropItems(ItemStackHandler inventoryHandler, Level level, BlockPos pos) {
		dropItems(inventoryHandler, level, pos.getX(), pos.getY(), pos.getZ());
	}

	public static void dropItems(ItemStackHandler inventoryHandler, Level level, double x, double y, double z) {
		iterate(inventoryHandler, (slot, stack) -> {
			Containers.dropItemStack(level, x, y, z, stack);
			inventoryHandler.setStackInSlot(slot, ItemStack.EMPTY);
		});
	}

	public static int getAnalogOutputSignal(IItemHandler handler) {
		AtomicDouble totalFilled = new AtomicDouble(0);
		AtomicBoolean isEmpty = new AtomicBoolean(true);
		iterate(handler, (slot, stack) -> {
			if (!stack.isEmpty()) {
				int slotLimit = handler.getSlotLimit(slot);
				totalFilled.addAndGet(stack.getCount() / (slotLimit / ((float) 64 / stack.getMaxStackSize())));
				isEmpty.set(false);
			}
		});
		double percentFilled = totalFilled.get() / handler.getSlots();
		return Mth.floor(percentFilled * 14.0F) + (isEmpty.get() ? 0 : 1);
	}
}
