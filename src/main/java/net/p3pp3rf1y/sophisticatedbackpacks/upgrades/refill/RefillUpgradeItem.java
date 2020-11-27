package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class RefillUpgradeItem extends UpgradeItemBase<RefillUpgradeWrapper> {
	private static final UpgradeType<RefillUpgradeWrapper> TYPE = new UpgradeType<>(RefillUpgradeWrapper::new);

	@Override
	public UpgradeType<RefillUpgradeWrapper> getType() {
		return TYPE;
	}
}
