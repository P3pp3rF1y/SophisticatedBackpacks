package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class FeedingUpgradeItem extends UpgradeItemBase<FeedingUpgradeWrapper> {
	public static final UpgradeType<FeedingUpgradeWrapper> TYPE = new UpgradeType<>(FeedingUpgradeWrapper::new);

	@Override
	public UpgradeType<FeedingUpgradeWrapper> getType() {
		return TYPE;
	}
}
