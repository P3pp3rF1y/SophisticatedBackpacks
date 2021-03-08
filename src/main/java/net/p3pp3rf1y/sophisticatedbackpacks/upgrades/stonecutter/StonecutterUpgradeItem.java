package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class StonecutterUpgradeItem extends UpgradeItemBase<StonecutterUpgradeWrapper> {
	private static final UpgradeType<StonecutterUpgradeWrapper> TYPE = new UpgradeType<>(StonecutterUpgradeWrapper::new);

	@Override
	public UpgradeType<StonecutterUpgradeWrapper> getType() {
		return TYPE;
	}
}
