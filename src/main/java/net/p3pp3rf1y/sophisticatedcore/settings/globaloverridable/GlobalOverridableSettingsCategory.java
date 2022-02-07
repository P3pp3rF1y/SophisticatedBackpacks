package net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.settings.GlobalOverridableSetting;
import net.p3pp3rf1y.sophisticatedcore.settings.ISettingsCategory;

import java.util.Optional;
import java.util.function.Consumer;

public class GlobalOverridableSettingsCategory implements ISettingsCategory {
	public static final String NAME = "global";
	private CompoundTag categoryNbt;
	private final Consumer<CompoundTag> saveNbt;

	private final String playerSettingsTagName;

	public GlobalOverridableSettingsCategory(CompoundTag categoryNbt, Consumer<CompoundTag> saveNbt, String playerSettingsTagName) {
		this.categoryNbt = categoryNbt;
		this.saveNbt = saveNbt;
		this.playerSettingsTagName = playerSettingsTagName;
	}

	public String getPlayerSettingsTagName() {
		return playerSettingsTagName;
	}

	public <T> Optional<T> getSettingValue(GlobalOverridableSetting<T> setting) {
		return setting.getValue(categoryNbt);
	}

	public <T> void setSettingValue(GlobalOverridableSetting<T> setting, T value) {
		setting.setValue(categoryNbt, value);
		saveNbt.accept(categoryNbt);
	}

	public <T> void removeSetting(GlobalOverridableSetting<T> setting) {
		setting.removeFrom(categoryNbt);
		saveNbt.accept(categoryNbt);
	}

	@Override
	public void reloadFrom(CompoundTag categoryNbt) {
		this.categoryNbt = categoryNbt;
	}
}
