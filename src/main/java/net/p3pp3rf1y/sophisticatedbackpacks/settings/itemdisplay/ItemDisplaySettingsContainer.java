package net.p3pp3rf1y.sophisticatedbackpacks.settings.itemdisplay;

import net.minecraft.item.DyeColor;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.SettingsContainerBase;

import java.util.Optional;

public class ItemDisplaySettingsContainer extends SettingsContainerBase<ItemDisplaySettingsCategory> {
	private static final String ACTION_TAG = "action";
	private static final String COLOR_TAG = "color";
	private static final String SELECT_SLOT_TAG = "selectSlot";
	private static final String UNSELECT_SLOT_ACTION = "unselectSlot";
	private static final String ROTATE_CLOCKWISE_ACTION = "rotateClockwise";
	private static final String ROTATE_COUNTER_CLOCKWISE_ACTION = "rotateCounterClockwise";

	public ItemDisplaySettingsContainer(SettingsContainer settingsContainer, String categoryName, ItemDisplaySettingsCategory category) {
		super(settingsContainer, categoryName, category);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(ACTION_TAG)) {
			switch (data.getString(ACTION_TAG)) {
				case UNSELECT_SLOT_ACTION:
					unselectSlot();
					break;
				case ROTATE_CLOCKWISE_ACTION:
					rotateClockwise();
					break;
				case ROTATE_COUNTER_CLOCKWISE_ACTION:
					rotateCounterClockwise();
					break;
				default:
			}
		} else if (data.contains(SELECT_SLOT_TAG)) {
			selectSlot(data.getInt(SELECT_SLOT_TAG));
		} else if (data.contains(COLOR_TAG)) {
			setColor(DyeColor.byId(data.getInt(COLOR_TAG)));
		}
	}

	public void unselectSlot(int slotNumber) {
		if (!isSlotSelected(slotNumber)) {
			return;
		}
		unselectSlot();
	}

	private void unselectSlot() {
		if (isServer()) {
			getCategory().unselectSlot();
		} else {
			sendStringToServer(ACTION_TAG, UNSELECT_SLOT_ACTION);
		}
	}

	public void selectSlot(int slotNumber) {
		if (isSlotSelected(slotNumber)) {
			return;
		}
		if (isServer()) {
			getCategory().selectSlot(slotNumber);
		} else {
			sendIntToServer(SELECT_SLOT_TAG, slotNumber);
		}
	}

	public void rotateClockwise() {
		if (isServer()) {
			getCategory().rotate(true);
		} else {
			sendStringToServer(ACTION_TAG, ROTATE_CLOCKWISE_ACTION);
		}
	}

	public void rotateCounterClockwise() {
		if (isServer()) {
			getCategory().rotate(false);
		} else {
			sendStringToServer(ACTION_TAG, ROTATE_COUNTER_CLOCKWISE_ACTION);
		}
	}

	public void setColor(DyeColor color) {
		if (isServer()) {
			getCategory().setColor(color);
		} else {
			sendIntToServer(COLOR_TAG, color.getId());
		}
	}

	public boolean isSlotSelected(int slotNumber) {
		return getCategory().getSlot().map(s -> s == slotNumber).orElse(false);
	}

	public DyeColor getColor() {
		return getCategory().getColor();
	}

	public int getRotation() {
		return getCategory().getRotation();
	}

	public Optional<Integer> getSlot() {
		return getCategory().getSlot();
	}
}
