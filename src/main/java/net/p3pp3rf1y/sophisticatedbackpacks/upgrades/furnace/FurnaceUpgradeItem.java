package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.furnace;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class FurnaceUpgradeItem extends UpgradeItemBase<FurnaceUpgradeWrapper> {
	public static final UpgradeType<FurnaceUpgradeWrapper> TYPE = new UpgradeType<>(FurnaceUpgradeWrapper::new);

	@Override
	public UpgradeType<FurnaceUpgradeWrapper> getType() {
		return TYPE;
	}
}
