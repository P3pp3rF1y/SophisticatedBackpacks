package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class SmeltingUpgradeItem extends UpgradeItemBase<CookingUpgradeWrapper.SmeltingUpgradeWrapper> {
	public static final UpgradeType<CookingUpgradeWrapper.SmeltingUpgradeWrapper> TYPE = new UpgradeType<>(CookingUpgradeWrapper.SmeltingUpgradeWrapper::new);

	@Override
	public UpgradeType<CookingUpgradeWrapper.SmeltingUpgradeWrapper> getType() {
		return TYPE;
	}
}
