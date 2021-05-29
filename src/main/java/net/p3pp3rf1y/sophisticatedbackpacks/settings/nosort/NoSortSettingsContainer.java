package net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort;

import net.minecraft.item.DyeColor;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.SettingsContainerBase;

public class NoSortSettingsContainer extends SettingsContainerBase<NoSortSettingsCategory> {

	private static final String ACTION_TAG = "action";
	private static final String SELECT_ALL_ACTION = "selectAll";
	private static final String UNSELECT_ALL_ACTION = "unselectAll";
	private static final String UNSELECT_SLOT_TAG = "unselectSlot";
	private static final String SELECT_SLOT_TAG = "selectSlot";
	private static final String COLOR_TAG = "color";

	public NoSortSettingsContainer(SlotSettingsContainer settingsContainer, String categoryName, NoSortSettingsCategory category) {
		super(settingsContainer, categoryName, category);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(ACTION_TAG)) {
			switch (data.getString(ACTION_TAG)) {
				case SELECT_ALL_ACTION:
					selectAllSlots();
					break;
				case UNSELECT_ALL_ACTION:
					unselectAllSlots();
					break;
				default:
			}
		} else if (data.contains(SELECT_SLOT_TAG)) {
			selectSlot(data.getInt(SELECT_SLOT_TAG));
		} else if (data.contains(UNSELECT_SLOT_TAG)) {
			unselectSlot(data.getInt(UNSELECT_SLOT_TAG));
		} else if (data.contains(COLOR_TAG)) {
			setColor(DyeColor.byId(data.getInt(COLOR_TAG)));
		}
	}

	public void unselectSlot(int slotNumber) {
		if (!isSlotSelected(slotNumber)) {
			return;
		}
		getCategory().unselectSlot(slotNumber);
		sendIntToServer(UNSELECT_SLOT_TAG, slotNumber);
	}

	public void selectSlot(int slotNumber) {
		if (isSlotSelected(slotNumber)) {
			return;
		}
		getCategory().selectSlot(slotNumber);
		sendIntToServer(SELECT_SLOT_TAG, slotNumber);
	}

	public void unselectAllSlots() {
		getCategory().unselectAllSlots();
		sendStringToServer(ACTION_TAG, UNSELECT_ALL_ACTION);
	}

	public void selectAllSlots() {
		getCategory().selectSlots(0, getSettingsContainer().getNumberOfSlots());
		sendStringToServer(ACTION_TAG, SELECT_ALL_ACTION);
	}

	public void setColor(DyeColor color) {
		getCategory().setColor(color);
		sendIntToServer(COLOR_TAG, color.getId());
	}

	public boolean isSlotSelected(int slotNumber) {
		return getCategory().isSlotSelected(slotNumber);
	}

	public DyeColor getColor() {
		return getCategory().getColor();
	}
}
