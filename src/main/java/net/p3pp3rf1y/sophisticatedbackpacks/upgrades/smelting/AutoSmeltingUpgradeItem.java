package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class AutoSmeltingUpgradeItem extends UpgradeItemBase<AutoSmeltingUpgradeWrapper> {
	public static final UpgradeType<AutoSmeltingUpgradeWrapper> TYPE = new UpgradeType<>(AutoSmeltingUpgradeWrapper::new);

	@Override
	public UpgradeType<AutoSmeltingUpgradeWrapper> getType() {
		return TYPE;
	}
}
