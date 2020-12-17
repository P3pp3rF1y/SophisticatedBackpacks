package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.function.IntSupplier;

public class DepositUpgradeItem extends UpgradeItemBase<DepositUpgradeWrapper> {
	private static final UpgradeType<DepositUpgradeWrapper> TYPE = new UpgradeType<>(DepositUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public DepositUpgradeItem(IntSupplier filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<DepositUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
