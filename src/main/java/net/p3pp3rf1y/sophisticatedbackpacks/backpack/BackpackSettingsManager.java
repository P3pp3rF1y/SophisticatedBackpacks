package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
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
			new BackpackSetting<>("shiftClickOpenTab", NBTHelper::getBoolean, CompoundNBT::putBoolean, true);
	public static final BackpackSetting<Boolean> KEEP_TAB_OPEN =
			new BackpackSetting<>("keepTabOpen", NBTHelper::getBoolean, CompoundNBT::putBoolean, true);

	static {
		settings.put(SHIFT_CLICK_INTO_OPEN_TAB_FIRST.getName(), SHIFT_CLICK_INTO_OPEN_TAB_FIRST);
		settings.put(KEEP_TAB_OPEN.getName(), KEEP_TAB_OPEN);
	}

	public static Optional<BackpackSetting<?>> getBackpackSetting(String settingName) {
		return Optional.ofNullable(settings.get(settingName));
	}

	public static <T> T getPlayerSettingOrDefault(PlayerEntity player, BackpackSetting<T> setting) {
		return getPlayerSetting(player, setting).orElse(setting.getDefaultValue());
	}

	public static <T> Optional<T> getPlayerSetting(PlayerEntity player, BackpackSetting<T> setting) {
		return setting.getValue(getPlayerBackpackSettingsTag(player));
	}

	public static CompoundNBT getPlayerBackpackSettingsTag(PlayerEntity player) {
		return player.getPersistentData().getCompound(SOPHISTICATED_BACKPACK_SETTINGS_TAG);
	}

	public static void setPlayerBackpackSettingsTag(PlayerEntity player, CompoundNBT settingsNbt) {
		player.getPersistentData().put(SOPHISTICATED_BACKPACK_SETTINGS_TAG, settingsNbt);
	}

	public static <T> void setPlayerSetting(PlayerEntity player, BackpackSetting<T> setting, T value) {
		if (!player.getPersistentData().contains(SOPHISTICATED_BACKPACK_SETTINGS_TAG)) {
			player.getPersistentData().put(SOPHISTICATED_BACKPACK_SETTINGS_TAG, new CompoundNBT());
		}
		if (value != setting.defaultValue) {
			setting.setValue(getPlayerBackpackSettingsTag(player), value);
		} else {
			setting.removeFrom(getPlayerBackpackSettingsTag(player));
		}
	}

	public static <T> void setBackpackSetting(PlayerEntity player, BackpackSettingsCategory category, BackpackSetting<T> setting, T value) {
		T playerSettingValue = getPlayerSetting(player, setting).orElse(setting.getDefaultValue());
		if (playerSettingValue != value) {
			category.setSettingValue(setting, value);
		} else {
			category.removeSetting(setting);
		}
	}

	public static <T> T getBackpackSettingValue(PlayerEntity player, BackpackSettingsCategory category, BackpackSetting<T> setting) {
		return category.getSettingValue(setting).orElse(getPlayerSetting(player, setting).orElse(setting.getDefaultValue()));
	}

	public static class BackpackSetting<T> {
		private final String tagName;
		private final BiFunction<CompoundNBT, String, Optional<T>> getValue;
		private final TriConsumer<CompoundNBT, String, T> setValue;
		private final T defaultValue;

		public BackpackSetting(String tagName, BiFunction<CompoundNBT, String, Optional<T>> getValue, TriConsumer<CompoundNBT, String, T> setValue, T defaultValue) {
			this.tagName = tagName;
			this.getValue = getValue;
			this.setValue = setValue;
			this.defaultValue = defaultValue;
		}

		public String getName() {
			return tagName;
		}

		public void setValue(CompoundNBT tag, T value) {
			setValue.accept(tag, tagName, value);
		}

		public void removeFrom(CompoundNBT tag) {
			tag.remove(tagName);
		}

		public Optional<T> getValue(CompoundNBT tag) {
			return getValue.apply(tag, tagName);
		}

		public T getDefaultValue() {
			return defaultValue;
		}
	}
}
