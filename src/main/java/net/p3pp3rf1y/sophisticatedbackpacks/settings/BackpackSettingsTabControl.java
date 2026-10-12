package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.Tab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.settings.StorageSettingsTabControlBase;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsTab;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsTab;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsTab;

public class BackpackSettingsTabControl extends StorageSettingsTabControlBase {

	public static void registerFactories() {
		addFactory(BackpackMainSettingsCategory.NAME, BackpackMainSettingsTab::new);
		addFactory(ItemDisplaySettingsCategory.NAME, ItemDisplaySettingsTab::new);
		addFactory(NoSortSettingsCategory.NAME, NoSortSettingsTab::new);
		addFactory(MemorySettingsCategory.NAME, MemorySettingsTab::new);
	}

	public BackpackSettingsTabControl(SettingsScreen screen, Position position) {
		super(screen, position);
	}

	@Override
	protected Tab instantiateReturnBackTab() {
		return new BackToBackpackTab(new Position(x, getTopY()));
	}

	@Override
	protected boolean isSettingsCategoryDisabled(String categoryName) {
		if (categoryName.equals(ItemDisplaySettingsCategory.NAME)) {
			return Boolean.TRUE.equals(Config.SERVER.itemDisplayDisabled.get());
		}
		return super.isSettingsCategoryDisabled(categoryName);
	}
}
