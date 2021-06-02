package net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort;

import net.minecraft.item.DyeColor;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISlotColorCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;

public class NoSortSettingsCategory implements ISettingsCategory, ISlotColorCategory {
	public static final String NAME = "no_sort";
	private static final String COLOR_TAG = "color";
	private static final String SELECTED_SLOTS_TAG = "selectedSlots";
	private CompoundNBT categoryNbt;
	private final Consumer<CompoundNBT> saveNbt;
	private final Set<Integer> selectedSlots = new HashSet<>();
	private DyeColor color = DyeColor.LIME;

	public NoSortSettingsCategory(CompoundNBT categoryNbt, Consumer<CompoundNBT> saveNbt) {
		this.categoryNbt = categoryNbt;
		this.saveNbt = saveNbt;

		deserialize();
	}

	private void deserialize() {
		for (int slotNumber : categoryNbt.getIntArray(SELECTED_SLOTS_TAG)) {
			selectedSlots.add(slotNumber);
		}
		NBTHelper.getInt(categoryNbt, COLOR_TAG).ifPresent(c -> color = DyeColor.byId(c));
	}

	public boolean isSlotSelected(int slotNumber) {
		return selectedSlots.contains(slotNumber);
	}

	public void unselectAllSlots() {
		selectedSlots.clear();
		serializeSelectedSlots();
	}

	/**
	 * Selects slots that shouldn't be sorted
	 *
	 * @param minSlot inclusive
	 * @param maxSlot exclusive
	 */

	public void selectSlots(int minSlot, int maxSlot) {
		for (int slot = minSlot; slot < maxSlot; slot++) {
			selectedSlots.add(slot);
		}
		serializeSelectedSlots();
	}

	public void selectSlot(int slotNumber) {
		selectSlots(slotNumber, slotNumber + 1);
	}

	public void unselectSlot(int slotNumber) {
		selectedSlots.remove(slotNumber);
		serializeSelectedSlots();
	}

	private void serializeSelectedSlots() {
		int[] slots = new int[selectedSlots.size()];
		int i = 0;
		for (int slotNumber : selectedSlots) {
			slots[i++] = slotNumber;
		}
		categoryNbt.putIntArray(SELECTED_SLOTS_TAG, slots);
		saveNbt.accept(categoryNbt);
	}

	public void setColor(DyeColor color) {
		this.color = color;
		categoryNbt.putInt(COLOR_TAG, color.getId());
		saveNbt.accept(categoryNbt);
	}

	public DyeColor getColor() {
		return color;
	}

	@Override
	public Optional<Integer> getSlotColor(int slotNumber) {
		return selectedSlots.contains(slotNumber) ? Optional.of(color.getColorValue()) : Optional.empty();
	}

	public Set<Integer> getNoSortSlots() {
		return selectedSlots;
	}

	@Override
	public void reloadFrom(CompoundNBT categoryNbt) {
		this.categoryNbt = categoryNbt;
		selectedSlots.clear();
		color = DyeColor.LIME;
		deserialize();
	}
}
