package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class InceptionUpgradeItem extends UpgradeItemBase<InceptionUpgradeWrapper> {
	public static final UpgradeType<InceptionUpgradeWrapper> TYPE = new UpgradeType<>(InceptionUpgradeWrapper::new);

	@Override
	public UpgradeType<InceptionUpgradeWrapper> getType() {
		return TYPE;
	}

}
