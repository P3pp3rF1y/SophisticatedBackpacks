package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.function.IntSupplier;

public class CompactingUpgradeItem extends UpgradeItemBase<CompactingUpgradeWrapper> {
	private static final UpgradeType<CompactingUpgradeWrapper> TYPE = new UpgradeType<>(CompactingUpgradeWrapper::new);
	private final boolean shouldCompactNineByNine;
	private final IntSupplier filterSlotCount;

	public CompactingUpgradeItem(boolean shouldCompactNineByNine, IntSupplier filterSlotCount) {
		this.shouldCompactNineByNine = shouldCompactNineByNine;
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<CompactingUpgradeWrapper> getType() {
		return TYPE;
	}

	public boolean shouldCompactNineByNine() {
		return shouldCompactNineByNine;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
