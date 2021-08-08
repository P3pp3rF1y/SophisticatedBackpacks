package net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISettingsCategory;

import java.util.Optional;
import java.util.function.Consumer;

public class BackpackSettingsCategory implements ISettingsCategory {
	public static final String NAME = "backpack";
	private CompoundNBT categoryNbt;
	private final Consumer<CompoundNBT> saveNbt;

	public BackpackSettingsCategory(CompoundNBT categoryNbt, Consumer<CompoundNBT> saveNbt) {
		this.categoryNbt = categoryNbt;
		this.saveNbt = saveNbt;
	}

	public <T> Optional<T> getSettingValue(BackpackSettingsManager.BackpackSetting<T> setting) {
		return setting.getValue(categoryNbt);
	}

	public <T> void setSettingValue(BackpackSettingsManager.BackpackSetting<T> setting, T value) {
		setting.setValue(categoryNbt, value);
		saveNbt.accept(categoryNbt);
	}

	public <T> void removeSetting(BackpackSettingsManager.BackpackSetting<T> setting) {
		setting.removeFrom(categoryNbt);
		saveNbt.accept(categoryNbt);
	}

	@Override
	public void reloadFrom(CompoundNBT categoryNbt) {
		this.categoryNbt = categoryNbt;
	}
}
