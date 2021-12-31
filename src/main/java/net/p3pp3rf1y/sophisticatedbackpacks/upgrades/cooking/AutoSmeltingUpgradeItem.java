package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class AutoSmeltingUpgradeItem extends UpgradeItemBase<AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper> {
	public static final UpgradeType<AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper> TYPE = new UpgradeType<>(AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper::new);

	@Override
	public UpgradeType<AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper> getType() {
		return TYPE;
	}
}
