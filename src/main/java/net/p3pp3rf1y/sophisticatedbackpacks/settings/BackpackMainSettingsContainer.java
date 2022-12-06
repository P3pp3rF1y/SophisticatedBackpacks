package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainerMenu;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsContainer;

public class BackpackMainSettingsContainer extends MainSettingsContainer {
	public BackpackMainSettingsContainer(SettingsContainerMenu<?> settingsContainer, String categoryName, MainSettingsCategory category) {
		super(settingsContainer, categoryName, category);
	}

	public void toggleAnotherPlayerCanOpen() {
		toggleBooleanSetting(getPlayer(), BackpackMainSettingsCategory.ANOTHER_PLAYER_CAN_OPEN);
	}

	public boolean canAnotherPlayerOpen() {
		return getSettingValue(BackpackMainSettingsCategory.ANOTHER_PLAYER_CAN_OPEN);
	}
}
