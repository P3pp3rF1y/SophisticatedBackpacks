package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.function.IntSupplier;

public class DepositUpgradeItem extends UpgradeItemBase<DepositUpgradeWrapper> {
	private static final UpgradeType<DepositUpgradeWrapper> TYPE = new UpgradeType<>(DepositUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public DepositUpgradeItem(IntSupplier filterSlotCount) {
		super(SophisticatedBackpacks.ITEM_GROUP, Config.SERVER.maxUpgradesPerStorage);
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
