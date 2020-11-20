package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class PickupUpgradeItem extends UpgradeItemBase<PickupUpgradeWrapper> {
	public static final UpgradeType<PickupUpgradeWrapper> TYPE = new UpgradeType<>(PickupUpgradeWrapper::new);

	private final int filterSlotCount;

	public PickupUpgradeItem() {
		this(9);
	}

	public PickupUpgradeItem(int filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	public int getFilterSlotCount() {
		return filterSlotCount;
	}

	@Override
	public UpgradeType<PickupUpgradeWrapper> getType() {
		return TYPE;
	}
}
