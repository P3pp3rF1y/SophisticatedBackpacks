package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class SmokingUpgradeItem extends UpgradeItemBase<CookingUpgradeWrapper.SmokingUpgradeWrapper> {
	public static final UpgradeType<CookingUpgradeWrapper.SmokingUpgradeWrapper> TYPE = new UpgradeType<>(CookingUpgradeWrapper.SmokingUpgradeWrapper::new);

	@Override
	public UpgradeType<CookingUpgradeWrapper.SmokingUpgradeWrapper> getType() {
		return TYPE;
	}
}
