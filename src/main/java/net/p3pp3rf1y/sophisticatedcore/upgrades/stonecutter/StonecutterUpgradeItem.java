package net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

public class StonecutterUpgradeItem extends UpgradeItemBase<StonecutterUpgradeWrapper> {
	private static final UpgradeType<StonecutterUpgradeWrapper> TYPE = new UpgradeType<>(StonecutterUpgradeWrapper::new);

	public StonecutterUpgradeItem(CreativeModeTab itemGroup) {super(itemGroup);}

	@Override
	public UpgradeType<StonecutterUpgradeWrapper> getType() {
		return TYPE;
	}
}
