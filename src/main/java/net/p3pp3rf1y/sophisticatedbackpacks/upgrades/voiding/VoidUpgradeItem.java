package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class VoidUpgradeItem extends UpgradeItemBase<VoidUpgradeWrapper> {
	private static final UpgradeType<VoidUpgradeWrapper> TYPE = new UpgradeType<>(VoidUpgradeWrapper::new);
	private final int filterSlotCount;

	public VoidUpgradeItem() {
		this(9);
	}

	public VoidUpgradeItem(int filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<VoidUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount;
	}
}
