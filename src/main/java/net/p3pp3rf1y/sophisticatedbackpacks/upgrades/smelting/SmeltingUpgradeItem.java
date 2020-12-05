package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class SmeltingUpgradeItem extends UpgradeItemBase<SmeltingUpgradeWrapper> {
	public static final UpgradeType<SmeltingUpgradeWrapper> TYPE = new UpgradeType<>(SmeltingUpgradeWrapper::new);

	@Override
	public UpgradeType<SmeltingUpgradeWrapper> getType() {
		return TYPE;
	}
}
