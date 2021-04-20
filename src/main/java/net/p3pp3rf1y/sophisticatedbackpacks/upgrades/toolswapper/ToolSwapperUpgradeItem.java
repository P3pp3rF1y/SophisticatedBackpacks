package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class ToolSwapperUpgradeItem extends UpgradeItemBase<ToolSwapperUpgradeWrapper> {
	private static final UpgradeType<ToolSwapperUpgradeWrapper> TYPE = new UpgradeType<>(ToolSwapperUpgradeWrapper::new);
	private final boolean hasSettingsTab;

	public ToolSwapperUpgradeItem(boolean hasSettingsTab) {this.hasSettingsTab = hasSettingsTab;}

	@Override
	public UpgradeType<ToolSwapperUpgradeWrapper> getType() {
		return TYPE;
	}

	public boolean hasSettingsTab() {
		return hasSettingsTab;
	}
}
