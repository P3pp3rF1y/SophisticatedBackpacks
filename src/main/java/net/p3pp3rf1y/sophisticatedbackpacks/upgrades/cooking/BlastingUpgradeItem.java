package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class BlastingUpgradeItem extends UpgradeItemBase<CookingUpgradeWrapper.BlastingUpgradeWrapper> {
	public static final UpgradeType<CookingUpgradeWrapper.BlastingUpgradeWrapper> TYPE = new UpgradeType<>(CookingUpgradeWrapper.BlastingUpgradeWrapper::new);

	@Override
	public UpgradeType<CookingUpgradeWrapper.BlastingUpgradeWrapper> getType() {
		return TYPE;
	}
}
