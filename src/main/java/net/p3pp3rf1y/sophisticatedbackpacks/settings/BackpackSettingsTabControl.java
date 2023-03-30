package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import com.google.common.collect.ImmutableMap;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.Tab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsContainerBase;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsTab;
import net.p3pp3rf1y.sophisticatedcore.settings.StorageSettingsTabControlBase;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsTab;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsTab;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsTab;

import java.util.Map;

public class BackpackSettingsTabControl extends StorageSettingsTabControlBase {
	private static final Map<String, ISettingsTabFactory<?, ?>> SETTINGS_TAB_FACTORIES;

	static {
		ImmutableMap.Builder<String, ISettingsTabFactory<?, ?>> builder = new ImmutableMap.Builder<>();
		addFactory(builder, BackpackMainSettingsCategory.NAME, BackpackMainSettingsTab::new);
		addFactory(builder, NoSortSettingsCategory.NAME, NoSortSettingsTab::new);
		addFactory(builder, MemorySettingsCategory.NAME, MemorySettingsTab::new);
		addFactory(builder, ItemDisplaySettingsCategory.NAME, ItemDisplaySettingsTab::new);
		SETTINGS_TAB_FACTORIES = builder.build();
	}

	public BackpackSettingsTabControl(SettingsScreen screen, Position position) {
		super(screen, position);
	}

	@Override
	protected Tab instantiateReturnBackTab() {
		return new BackToBackpackTab(new Position(x, getTopY()));
	}

	@Override
	protected <C extends SettingsContainerBase<?>, T extends SettingsTab<C>> ISettingsTabFactory<C, T> getSettingsTabFactory(String name) {
		//noinspection unchecked
		return (ISettingsTabFactory<C, T>) SETTINGS_TAB_FACTORIES.get(name);
	}

	@Override
	protected boolean isSettingsCategoryDisabled(String categoryName) {
		if (categoryName.equals(ItemDisplaySettingsCategory.NAME)) {
			return Boolean.TRUE.equals(Config.SERVER.itemDisplayDisabled.get());
		}
		return super.isSettingsCategoryDisabled(categoryName);
	}
}
