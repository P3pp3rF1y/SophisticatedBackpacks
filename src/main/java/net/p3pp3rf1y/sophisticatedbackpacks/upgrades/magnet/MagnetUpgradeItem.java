package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.function.IntSupplier;

public class MagnetUpgradeItem extends UpgradeItemBase<MagnetUpgradeWrapper> {
	public static final UpgradeType<MagnetUpgradeWrapper> TYPE = new UpgradeType<>(MagnetUpgradeWrapper::new);
	private final IntSupplier radius;
	private final IntSupplier filterSlotCount;

	public MagnetUpgradeItem(IntSupplier radius, IntSupplier filterSlotCount) {
		this.radius = radius;
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<MagnetUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}

	public int getRadius() {
		return radius.getAsInt();
	}
}
