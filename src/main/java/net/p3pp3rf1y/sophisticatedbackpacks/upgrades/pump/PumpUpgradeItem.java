package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pump;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class PumpUpgradeItem extends UpgradeItemBase<PumpUpgradeWrapper> {
	private static final UpgradeType<PumpUpgradeWrapper> TYPE = new UpgradeType<>(PumpUpgradeWrapper::new);
	private final boolean interactWithHandDefault;
	private final boolean interactWithWorldDefault;

	public PumpUpgradeItem(boolean interactWithHandDefault, boolean interactWithWorldDefault) {
		this.interactWithHandDefault = interactWithHandDefault;
		this.interactWithWorldDefault = interactWithWorldDefault;
	}

	@Override
	public UpgradeType<PumpUpgradeWrapper> getType() {
		return TYPE;
	}

	public boolean getInteractWithHandDefault() {
		return interactWithHandDefault;
	}

	public boolean getInteractWithWorldDefault() {
		return interactWithWorldDefault;
	}
}
