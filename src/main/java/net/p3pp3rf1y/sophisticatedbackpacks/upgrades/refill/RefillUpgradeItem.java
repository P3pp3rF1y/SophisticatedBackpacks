package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

public class RefillUpgradeItem extends UpgradeItemBase<RefillUpgradeWrapper> {
	private static final UpgradeType<RefillUpgradeWrapper> TYPE = new UpgradeType<>(RefillUpgradeWrapper::new);

	public RefillUpgradeItem() {super(SophisticatedBackpacks.ITEM_GROUP);}

	@Override
	public UpgradeType<RefillUpgradeWrapper> getType() {
		return TYPE;
	}
}
