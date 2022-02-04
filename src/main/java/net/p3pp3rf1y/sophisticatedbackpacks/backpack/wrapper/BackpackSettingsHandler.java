package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;

public class BackpackSettingsHandler extends SettingsHandler {
	public static final String SOPHISTICATED_BACKPACK_SETTINGS_PLAYER_TAG = "sophisticatedBackpackSettings";

	public BackpackSettingsHandler(IStorageWrapper backpackWrapper, CompoundTag backpackContentsNbt, Runnable markBackpackContentsDirty) {
		super(backpackContentsNbt, markBackpackContentsDirty, SOPHISTICATED_BACKPACK_SETTINGS_PLAYER_TAG, backpackWrapper::getInventoryHandler, backpackWrapper::getRenderInfo);
	}

	public void copyTo(SettingsHandler settingsHandler) {
		if (contentsNbt.contains(SETTINGS_TAG)) {
			//noinspection ConstantConditions - checking for whether tag exists just one line up
			settingsHandler.getNbt().put(SETTINGS_TAG, contentsNbt.get(SETTINGS_TAG));
		}
	}

	public void reloadFrom(CompoundTag backpackContentsNbt) {
		CompoundTag settingsNbt = backpackContentsNbt.getCompound(SETTINGS_TAG);
		getSettingsCategories().forEach((categoryName, category) -> category.reloadFrom(settingsNbt.getCompound(categoryName)));
	}
}
