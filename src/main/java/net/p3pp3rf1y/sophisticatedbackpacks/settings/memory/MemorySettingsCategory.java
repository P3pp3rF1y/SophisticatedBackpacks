package net.p3pp3rf1y.sophisticatedbackpacks.settings.memory;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Consumer;

public class MemorySettingsCategory implements ISettingsCategory {
	public static final String NAME = "memory";
	private static final String SLOT_FILTER_ITEMS_TAG = "slotFilterItems";
	private final IBackpackWrapper backpackWrapper;
	private CompoundNBT categoryNbt;
	private final Consumer<CompoundNBT> saveNbt;
	private final Map<Integer, Item> slotFilterItems = new TreeMap<>();
	private final Map<Item, Set<Integer>> filterItemSlots = new HashMap<>();

	public MemorySettingsCategory(IBackpackWrapper backpackWrapper, CompoundNBT categoryNbt, Consumer<CompoundNBT> saveNbt) {
		this.backpackWrapper = backpackWrapper;
		this.categoryNbt = categoryNbt;
		this.saveNbt = saveNbt;

		deserialize();
	}

	private void deserialize() {
		//TODO remove this legacy thing in the future - only included here as the tag below is no longer used
		if (categoryNbt.contains("slotFilterStacks")) {
			categoryNbt.remove("slotFilterStacks");
			saveNbt.accept(categoryNbt);
		}

		NBTHelper.getMap(categoryNbt.getCompound(SLOT_FILTER_ITEMS_TAG),
						Integer::valueOf,
						(k, v) -> Optional.ofNullable(ForgeRegistries.ITEMS.getValue(new ResourceLocation(v.getAsString()))))
				.ifPresent(map -> map.forEach(this::addSlotItem));
	}

	public boolean matchesFilter(int slotNumber, ItemStack stack) {
		if (!slotFilterItems.containsKey(slotNumber)) {
			return true;
		}

		return !stack.isEmpty() && stack.getItem() == slotFilterItems.get(slotNumber);
	}

	public Optional<Item> getSlotFilterItem(int slotNumber) {
		return Optional.ofNullable(slotFilterItems.get(slotNumber));
	}

	public boolean isSlotSelected(int slotNumber) {
		return slotFilterItems.containsKey(slotNumber);
	}

	public void unselectAllSlots() {
		slotFilterItems.clear();
		filterItemSlots.clear();
		serializeFilterItems();
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
					Item item = stackInSlot.getItem();
					addSlotItem(slot, item);
				}
			}
		}
		serializeFilterItems();
	}

	private void addSlotItem(int slot, Item item) {
		slotFilterItems.put(slot, item);
		filterItemSlots.computeIfAbsent(item, k -> new TreeSet<>()).add(slot);
	}

	public void selectSlot(int slotNumber) {
		selectSlots(slotNumber, slotNumber + 1);
	}

	public void unselectSlot(int slotNumber) {
		Item item = slotFilterItems.remove(slotNumber);
		Set<Integer> itemSlots = filterItemSlots.get(item);
		itemSlots.remove(slotNumber);
		if (itemSlots.isEmpty()) {
			filterItemSlots.remove(item);
		}
		serializeFilterItems();
	}

	private void serializeFilterItems() {
		//noinspection ConstantConditions - item registry name exists in this content otherwise player wouldn't be able to work with it
		NBTHelper.putMap(categoryNbt, SLOT_FILTER_ITEMS_TAG, slotFilterItems, String::valueOf, i -> StringNBT.valueOf(i.getRegistryName().toString()));
		saveNbt.accept(categoryNbt);
	}

	@Override
	public void reloadFrom(CompoundNBT categoryNbt) {
		this.categoryNbt = categoryNbt;
		slotFilterItems.clear();
		filterItemSlots.clear();
		deserialize();
	}

	public Set<Integer> getSlotIndexes() {
		return slotFilterItems.keySet();
	}

	public Map<Item, Set<Integer>> getFilterItemSlots() {
		return filterItemSlots;
	}
}
