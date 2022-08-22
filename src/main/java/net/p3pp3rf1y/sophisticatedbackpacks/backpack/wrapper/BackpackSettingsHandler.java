package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;

public class BackpackSettingsHandler extends SettingsHandler {
	public static final String SETTINGS_TAG = "settings";

	public BackpackSettingsHandler(IStorageWrapper backpackWrapper, CompoundTag backpackContentsNbt, Runnable markBackpackContentsDirty) {
		super(backpackContentsNbt, markBackpackContentsDirty, backpackWrapper::getInventoryHandler, backpackWrapper::getRenderInfo);
	}

	public void copyTo(SettingsHandler settingsHandler) {
		if (contentsNbt.contains(SETTINGS_TAG)) {
			//noinspection ConstantConditions - checking for whether tag exists just one line up
			settingsHandler.getNbt().put(SETTINGS_TAG, contentsNbt.get(SETTINGS_TAG));
		}
	}

	@Override
	protected CompoundTag getSettingsNbtFromContentsNbt(CompoundTag contentsNbt)  {
		return contentsNbt.getCompound(SETTINGS_TAG);
	}

	@Override
	protected void saveCategoryNbt(CompoundTag settingsNbt, String categoryName, CompoundTag tag) {
		settingsNbt.put(categoryName, tag);
		contentsNbt.put(SETTINGS_TAG, settingsNbt);
	}

	@Override
	protected void addGlobalSettingsCategory(CompoundTag settingsNbt) {
		addSettingsCategory(settingsNbt, BackpackMainSettingsCategory.NAME, markContentsDirty, (categoryNbt, saveNbt) -> new BackpackMainSettingsCategory(categoryNbt, saveNbt));
	}

	@Override
	public BackpackMainSettingsCategory getGlobalSettingsCategory() {
		return getTypeCategory(BackpackMainSettingsCategory.class);
	}
}
