package net.p3pp3rf1y.sophisticatedbackpacks.settings.memory;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemStackHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

public class MemorySettingsCategory implements ISettingsCategory {
	public static final String NAME = "memory";
	private static final String SLOT_FILTER_STACKS_TAG = "slotFilterStacks";
	private final IBackpackWrapper backpackWrapper;
	private CompoundNBT categoryNbt;
	private final Consumer<CompoundNBT> saveNbt;
	private final Map<Integer, ItemStack> slotFilterStacks = new HashMap<>();

	public MemorySettingsCategory(IBackpackWrapper backpackWrapper, CompoundNBT categoryNbt, Consumer<CompoundNBT> saveNbt) {
		this.backpackWrapper = backpackWrapper;
		this.categoryNbt = categoryNbt;
		this.saveNbt = saveNbt;

		deserialize();
	}

	private void deserialize() {
		NBTHelper.getMap(categoryNbt.getCompound(SLOT_FILTER_STACKS_TAG), Integer::valueOf, (k, v) -> ItemStack.of((CompoundNBT) v)).ifPresent(slotFilterStacks::putAll);
	}

	public boolean matchesFilter(int slotNumber, ItemStack stack) {
		if (!slotFilterStacks.containsKey(slotNumber)) {
			return true;
		}

		ItemStack filterStack = slotFilterStacks.get(slotNumber);
		if (stack.isEmpty() || !stack.sameItem(filterStack) || stack.hasTag() != filterStack.hasTag()) {
			return false;
		}

		return ItemStackHelper.areItemStackTagsEqualIgnoreDurability(stack, filterStack);
	}

	public Optional<ItemStack> getSlotFilterStack(int slotNumber) {
		return Optional.ofNullable(slotFilterStacks.get(slotNumber));
	}

	public boolean isSlotSelected(int slotNumber) {
		return slotFilterStacks.containsKey(slotNumber);
	}

	public void unselectAllSlots() {
		slotFilterStacks.clear();
		serializeFilterStacks();
	}

	/**
	 * Selects slots that shouldn't be sorted
	 *
	 * @param minSlot inclusive
	 * @param maxSlot exclusive
	 */

	public void selectSlots(int minSlot, int maxSlot) {
		for (int slot = minSlot; slot < maxSlot; slot++) {
			BackpackInventoryHandler inventoryHandler = backpackWrapper.getInventoryHandler();
			if (slot < inventoryHandler.getSlots()) {
				ItemStack stackInSlot = inventoryHandler.getStackInSlot(slot);
				if (!stackInSlot.isEmpty()) {
					ItemStack copy = stackInSlot.copy();
					copy.setCount(1);
					slotFilterStacks.put(slot, copy);
				}
			}
		}
		serializeFilterStacks();
	}

	public void selectSlot(int slotNumber) {
		selectSlots(slotNumber, slotNumber + 1);
	}

	public void unselectSlot(int slotNumber) {
		slotFilterStacks.remove(slotNumber);
		serializeFilterStacks();
	}

	private void serializeFilterStacks() {
		NBTHelper.putMap(categoryNbt, SLOT_FILTER_STACKS_TAG, slotFilterStacks, String::valueOf, s -> s.save(new CompoundNBT()));
		saveNbt.accept(categoryNbt);
	}

	@Override
	public void reloadFrom(CompoundNBT categoryNbt) {
		this.categoryNbt = categoryNbt;
		slotFilterStacks.clear();
		deserialize();
	}
}
