package net.p3pp3rf1y.sophisticatedcore.settings;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable.GlobalOverridableSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class SettingsManager {
	private SettingsManager() {}

	private static final Map<String, GlobalOverridableSetting<?>> settings = new HashMap<>();

	public static final GlobalOverridableSetting<Boolean> SHIFT_CLICK_INTO_OPEN_TAB_FIRST =
			new GlobalOverridableSetting<>("shiftClickOpenTab", NBTHelper::getBoolean, CompoundTag::putBoolean, true);
	public static final GlobalOverridableSetting<Boolean> KEEP_TAB_OPEN =
			new GlobalOverridableSetting<>("keepTabOpen", NBTHelper::getBoolean, CompoundTag::putBoolean, true);

	static {
		settings.put(SHIFT_CLICK_INTO_OPEN_TAB_FIRST.getName(), SHIFT_CLICK_INTO_OPEN_TAB_FIRST);
		settings.put(KEEP_TAB_OPEN.getName(), KEEP_TAB_OPEN);
	}

	public static Optional<GlobalOverridableSetting<?>> getSetting(String settingName) {
		return Optional.ofNullable(settings.get(settingName));
	}

	public static <T> T getPlayerSettingOrDefault(Player player, String playerSettingsTagName, GlobalOverridableSetting<T> setting) {
		return getPlayerSetting(player, playerSettingsTagName, setting).orElse(setting.getDefaultValue());
	}

	public static <T> Optional<T> getPlayerSetting(Player player, String playerSettingsTagName, GlobalOverridableSetting<T> setting) {
		return setting.getValue(getPlayerSettingsTag(player, playerSettingsTagName));
	}

	public static CompoundTag getPlayerSettingsTag(Player player, String playerSettingsTagName) {
		return player.getPersistentData().getCompound(playerSettingsTagName);
	}

	public static void setPlayerSettingsTag(Player player, String playerSettingsTagName, CompoundTag settingsNbt) {
		player.getPersistentData().put(playerSettingsTagName, settingsNbt);
	}

	public static <T> void setPlayerSetting(Player player, String playerSettingsTagName, GlobalOverridableSetting<T> setting, T value) {
		if (!player.getPersistentData().contains(playerSettingsTagName)) {
			player.getPersistentData().put(playerSettingsTagName, new CompoundTag());
		}
		if (value != setting.getDefaultValue()) {
			setting.setValue(getPlayerSettingsTag(player, playerSettingsTagName), value);
		} else {
			setting.removeFrom(getPlayerSettingsTag(player, playerSettingsTagName));
		}
	}

	public static <T> void setSetting(Player player, String playerSettingsTagName, GlobalOverridableSettingsCategory category, GlobalOverridableSetting<T> setting, T value) {
		T playerSettingValue = getPlayerSetting(player, playerSettingsTagName, setting).orElse(setting.getDefaultValue());
		if (playerSettingValue != value) {
			category.setSettingValue(setting, value);
		} else {
			category.removeSetting(setting);
		}
	}

	public static <T> T getSettingValue(Player player, String playerSettingsTagName, GlobalOverridableSettingsCategory category, GlobalOverridableSetting<T> setting) {
		return category.getSettingValue(setting).orElse(getPlayerSetting(player, playerSettingsTagName, setting).orElse(setting.getDefaultValue()));
	}

}
