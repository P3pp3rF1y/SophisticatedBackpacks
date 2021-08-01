package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class BatteryUpgradeItem extends UpgradeItemBase<BatteryUpgradeWrapper> {
	public UpgradeType<BatteryUpgradeWrapper> TYPE = new UpgradeType<>(BatteryUpgradeWrapper::new);

	@Override
	public UpgradeType<BatteryUpgradeWrapper> getType() {
		return TYPE;
	}
}
