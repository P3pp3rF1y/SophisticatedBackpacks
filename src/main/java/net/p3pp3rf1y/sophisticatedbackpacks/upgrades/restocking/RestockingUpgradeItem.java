package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restocking;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class RestockingUpgradeItem extends UpgradeItemBase<RestockingUpgradeWrapper> {
	private static final UpgradeType<RestockingUpgradeWrapper> TYPE = new UpgradeType<>(RestockingUpgradeWrapper::new);
	private final int filterSlotCount;

	public RestockingUpgradeItem() {
		this(9);
	}

	public RestockingUpgradeItem(int filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<RestockingUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount;
	}
}
