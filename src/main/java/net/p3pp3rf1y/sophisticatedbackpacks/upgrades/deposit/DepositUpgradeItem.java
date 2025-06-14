package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;
import java.util.function.IntSupplier;

public class DepositUpgradeItem extends UpgradeItemBase<DepositUpgradeWrapper> {
	private static final UpgradeType<DepositUpgradeWrapper> TYPE = new UpgradeType<>(DepositUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public DepositUpgradeItem(IntSupplier filterSlotCount, Properties properties) {
		super(Config.SERVER.maxUpgradesPerStorage, properties);
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<DepositUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return List.of();
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
