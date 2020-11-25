package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.depositing.restocking;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class DepositUpgradeItem extends UpgradeItemBase<DepositUpgradeWrapper> {
	private static final UpgradeType<DepositUpgradeWrapper> TYPE = new UpgradeType<>(DepositUpgradeWrapper::new);
	private final int filterSlotCount;

	public DepositUpgradeItem() {
		this(9);
	}

	public DepositUpgradeItem(int filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<DepositUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount;
	}
}
