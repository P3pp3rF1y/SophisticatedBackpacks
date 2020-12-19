package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.function.IntSupplier;

public class VoidUpgradeItem extends UpgradeItemBase<VoidUpgradeWrapper> {
	private static final UpgradeType<VoidUpgradeWrapper> TYPE = new UpgradeType<>(VoidUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public VoidUpgradeItem(IntSupplier filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<VoidUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
