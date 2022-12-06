package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;

import java.util.function.Consumer;
import java.util.function.Supplier;

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
	protected CompoundTag getSettingsNbtFromContentsNbt(CompoundTag contentsNbt) {
		return contentsNbt.getCompound(SETTINGS_TAG);
	}

	@Override
	protected void addItemDisplayCategory(Supplier<InventoryHandler> inventoryHandlerSupplier, Supplier<RenderInfo> renderInfoSupplier, CompoundTag settingsNbt) {
		addSettingsCategory(settingsNbt, ItemDisplaySettingsCategory.NAME, markContentsDirty, (categoryNbt, saveNbt) ->
				new ItemDisplaySettingsCategory(inventoryHandlerSupplier, renderInfoSupplier, categoryNbt, saveNbt, 1, () -> getTypeCategory(MemorySettingsCategory.class)));
	}

	@Override
	protected void saveCategoryNbt(CompoundTag settingsNbt, String categoryName, CompoundTag tag) {
		settingsNbt.put(categoryName, tag);
		contentsNbt.put(SETTINGS_TAG, settingsNbt);
	}

	@Override
	public String getGlobalSettingsCategoryName() {
		return BackpackMainSettingsCategory.NAME;
	}

	@Override
	public ISettingsCategory instantiateGlobalSettingsCategory(CompoundTag categoryNbt, Consumer<CompoundTag> saveNbt) {
		return new BackpackMainSettingsCategory(categoryNbt, saveNbt);
	}

	@Override
	public BackpackMainSettingsCategory getGlobalSettingsCategory() {
		return getTypeCategory(BackpackMainSettingsCategory.class);
	}
}
