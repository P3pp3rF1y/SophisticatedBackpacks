package net.p3pp3rf1y.sophisticatedcore.upgrades.xppump;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

public class XpPumpUpgradeItem extends UpgradeItemBase<XpPumpUpgradeWrapper> {
	public static final UpgradeType<XpPumpUpgradeWrapper> TYPE = new UpgradeType<>(XpPumpUpgradeWrapper::new);
	private final XpPumpUpgradeConfig xpPumpUpgradeConfig;

	public XpPumpUpgradeItem(CreativeModeTab itemGroup, XpPumpUpgradeConfig xpPumpUpgradeConfig) {super(itemGroup);
		this.xpPumpUpgradeConfig = xpPumpUpgradeConfig;
	}

	public XpPumpUpgradeConfig getXpPumpUpgradeConfig() {
		return xpPumpUpgradeConfig;
	}

	@Override
	public UpgradeType<XpPumpUpgradeWrapper> getType() {
		return TYPE;
	}
}
