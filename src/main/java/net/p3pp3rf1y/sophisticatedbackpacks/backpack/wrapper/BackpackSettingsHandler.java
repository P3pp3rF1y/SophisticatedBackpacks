package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategoryData;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;

import java.util.function.Supplier;

public class BackpackSettingsHandler extends SettingsHandler {
	public static final String SETTINGS_TAG = "settings";

	public BackpackSettingsHandler(IStorageWrapper backpackWrapper, ContainerContents.SettingsData settingsData, Runnable markBackpackContentsDirty) {
		super(settingsData, markBackpackContentsDirty, backpackWrapper::getInventoryHandler, backpackWrapper::getRenderDataHandler,
				SophisticatedBackpacks.MOD_ID);
	}

	@Override
	protected void addItemDisplayCategory(Supplier<InventoryHandler> inventoryHandlerSupplier, Supplier<RenderDataHandler> renderDataHandlerSupplier,
			ContainerContents.SettingsData settingsData) {
		this.<ItemDisplaySettingsCategoryData, ItemDisplaySettingsCategory>addSettingsCategory(settingsData, ItemDisplaySettingsCategory.NAME,
				markContentsDirty, (data, save) -> new ItemDisplaySettingsCategory(inventoryHandlerSupplier, renderDataHandlerSupplier, data, save, 1,
						() -> getTypeCategory(MemorySettingsCategory.class)),
				ItemDisplaySettingsCategoryData::new);
	}

	public void copyTo(SettingsHandler settingsHandler) {
		settingsHandler.reloadFrom(settingsHandler.getSettingsData());
	}
}
