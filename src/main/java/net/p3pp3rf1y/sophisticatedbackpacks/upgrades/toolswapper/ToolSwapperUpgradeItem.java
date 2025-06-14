package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;

public class ToolSwapperUpgradeItem extends UpgradeItemBase<ToolSwapperUpgradeWrapper> {
	private static final UpgradeType<ToolSwapperUpgradeWrapper> TYPE = new UpgradeType<>(ToolSwapperUpgradeWrapper::new);
	public static final List<UpgradeConflictDefinition> UPGRADE_CONFLICT_DEFINITIONS = List.of(new UpgradeConflictDefinition(ToolSwapperUpgradeItem.class::isInstance, 0, BackpackTranslationHelper.INSTANCE.translError("add.tool_swapper_exists")));
	private final boolean hasSettingsTab;
	private final boolean swapToolOnKeyPress;

	public ToolSwapperUpgradeItem(boolean hasSettingsTab, boolean swapToolOnKeyPress, Properties properties) {
		super(Config.SERVER.maxUpgradesPerStorage, properties);
		this.hasSettingsTab = hasSettingsTab;
		this.swapToolOnKeyPress = swapToolOnKeyPress;
	}

	@Override
	public UpgradeType<ToolSwapperUpgradeWrapper> getType() {
		return TYPE;
	}

	public boolean hasSettingsTab() {
		return hasSettingsTab;
	}

	public boolean shouldSwapToolOnKeyPress() {
		return swapToolOnKeyPress;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return UPGRADE_CONFLICT_DEFINITIONS;
	}
}
