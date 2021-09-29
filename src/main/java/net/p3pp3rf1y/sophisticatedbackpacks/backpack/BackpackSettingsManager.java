package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import org.apache.logging.log4j.util.TriConsumer;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;

public class BackpackSettingsManager {
	private BackpackSettingsManager() {}

	private static final String SOPHISTICATED_BACKPACK_SETTINGS_TAG = "sophisticatedBackpackSettings";

	private static final Map<String, BackpackSetting<?>> settings = new HashMap<>();

	public static final BackpackSetting<Boolean> SHIFT_CLICK_INTO_OPEN_TAB_FIRST =
			new BackpackSetting<>("shiftClickOpenTab", NBTHelper::getBoolean, CompoundTag::putBoolean, true);
	public static final BackpackSetting<Boolean> KEEP_TAB_OPEN =
			new BackpackSetting<>("keepTabOpen", NBTHelper::getBoolean, CompoundTag::putBoolean, true);

	static {
		settings.put(SHIFT_CLICK_INTO_OPEN_TAB_FIRST.getName(), SHIFT_CLICK_INTO_OPEN_TAB_FIRST);
		settings.put(KEEP_TAB_OPEN.getName(), KEEP_TAB_OPEN);
	}

	public static Optional<BackpackSetting<?>> getBackpackSetting(String settingName) {
		return Optional.ofNullable(settings.get(settingName));
	}

	public static <T> T getPlayerSettingOrDefault(Player player, BackpackSetting<T> setting) {
		return getPlayerSetting(player, setting).orElse(setting.getDefaultValue());
	}

	public static <T> Optional<T> getPlayerSetting(Player player, BackpackSetting<T> setting) {
		return setting.getValue(getPlayerBackpackSettingsTag(player));
	}

	public static CompoundTag getPlayerBackpackSettingsTag(Player player) {
		return player.getPersistentData().getCompound(SOPHISTICATED_BACKPACK_SETTINGS_TAG);
	}

	public static void setPlayerBackpackSettingsTag(Player player, CompoundTag settingsNbt) {
		player.getPersistentData().put(SOPHISTICATED_BACKPACK_SETTINGS_TAG, settingsNbt);
	}

	public static <T> void setPlayerSetting(Player player, BackpackSetting<T> setting, T value) {
		if (!player.getPersistentData().contains(SOPHISTICATED_BACKPACK_SETTINGS_TAG)) {
			player.getPersistentData().put(SOPHISTICATED_BACKPACK_SETTINGS_TAG, new CompoundTag());
		}
		if (value != setting.defaultValue) {
			setting.setValue(getPlayerBackpackSettingsTag(player), value);
		} else {
			setting.removeFrom(getPlayerBackpackSettingsTag(player));
		}
	}

	public static <T> void setBackpackSetting(Player player, BackpackSettingsCategory category, BackpackSetting<T> setting, T value) {
		T playerSettingValue = getPlayerSetting(player, setting).orElse(setting.getDefaultValue());
		if (playerSettingValue != value) {
			category.setSettingValue(setting, value);
		} else {
			category.removeSetting(setting);
		}
	}

	public static <T> T getBackpackSettingValue(Player player, BackpackSettingsCategory category, BackpackSetting<T> setting) {
		return category.getSettingValue(setting).orElse(getPlayerSetting(player, setting).orElse(setting.getDefaultValue()));
	}

	public static class BackpackSetting<T> {
		private final String tagName;
		private final BiFunction<CompoundTag, String, Optional<T>> getValue;
		private final TriConsumer<CompoundTag, String, T> setValue;
		private final T defaultValue;

		public BackpackSetting(String tagName, BiFunction<CompoundTag, String, Optional<T>> getValue, TriConsumer<CompoundTag, String, T> setValue, T defaultValue) {
			this.tagName = tagName;
			this.getValue = getValue;
			this.setValue = setValue;
			this.defaultValue = defaultValue;
		}

		public String getName() {
			return tagName;
		}

		public void setValue(CompoundTag tag, T value) {
			setValue.accept(tag, tagName, value);
		}

		public void removeFrom(CompoundTag tag) {
			tag.remove(tagName);
		}

		public Optional<T> getValue(CompoundTag tag) {
			return getValue.apply(tag, tagName);
		}

		public T getDefaultValue() {
			return defaultValue;
		}
	}
}
