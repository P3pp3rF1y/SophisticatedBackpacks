package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.xppump;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class XpPumpUpgradeItem extends UpgradeItemBase<XpPumpUpgradeWrapper> {
	public static final UpgradeType<XpPumpUpgradeWrapper> TYPE = new UpgradeType<>(XpPumpUpgradeWrapper::new);

	@Override
	public UpgradeType<XpPumpUpgradeWrapper> getType() {
		return TYPE;
	}
}
