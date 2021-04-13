package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class ToolSwapperUpgradeItem extends UpgradeItemBase<ToolSwapperUpgradeWrapper> {
	private static final UpgradeType<ToolSwapperUpgradeWrapper> TYPE = new UpgradeType<>(ToolSwapperUpgradeWrapper::new);

	@Override
	public UpgradeType<ToolSwapperUpgradeWrapper> getType() {
		return TYPE;
	}

}
