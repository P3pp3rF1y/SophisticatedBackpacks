package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class MagnetUpgradeItem extends UpgradeItemBase<MagnetUpgradeWrapper> {
	public static final UpgradeType<MagnetUpgradeWrapper> TYPE = new UpgradeType<>(MagnetUpgradeWrapper::new);
	private final int radius;
	private final int filterSlotCount;

	public MagnetUpgradeItem() {
		this(3, 9);
	}

	public MagnetUpgradeItem(int radius, int filterSlotCount) {
		this.radius = radius;
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<MagnetUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount;
	}

	public int getRadius() {
		return radius;
	}
}
