package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class RestockUpgradeItem extends UpgradeItemBase<RestockUpgradeWrapper> {
	private static final UpgradeType<RestockUpgradeWrapper> TYPE = new UpgradeType<>(RestockUpgradeWrapper::new);
	private final int filterSlotCount;

	public RestockUpgradeItem() {
		this(9);
	}

	public RestockUpgradeItem(int filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<RestockUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount;
	}
}
