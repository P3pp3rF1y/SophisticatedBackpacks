package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainerMenu;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategoryData;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsContainer;

public class BackpackMainSettingsContainer extends MainSettingsContainer {
	private static final String ANOTHER_PLAYER_CAN_OPEN_TAG = "anotherPlayerCanOpen";

	public BackpackMainSettingsContainer(SettingsContainerMenu<?> settingsContainer, String categoryName, MainSettingsCategory category) {
		super(settingsContainer, categoryName, category);
	}

	@Override
	public void handlePacket(CompoundTag data) {
		super.handlePacket(data);
		data.getBoolean(ANOTHER_PLAYER_CAN_OPEN_TAG).ifPresent(this::setAnotherPlayerCanOpen);
	}

	public void toggleAnotherPlayerCanOpen() {
		boolean value = !canAnotherPlayerOpen();
		setAnotherPlayerCanOpen(value);
	}

	private void setAnotherPlayerCanOpen(boolean value) {
		setSettingValue(data -> data.setAnotherPlayerCanOpen(value), tag -> tag.putBoolean(ANOTHER_PLAYER_CAN_OPEN_TAG, value));
	}

	public boolean canAnotherPlayerOpen() {
		return getSettingValue(MainSettingsCategoryData::anotherPlayerCanOpen);
	}
}
