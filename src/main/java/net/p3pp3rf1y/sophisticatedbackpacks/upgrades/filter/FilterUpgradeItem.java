package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.function.IntSupplier;

public class FilterUpgradeItem extends UpgradeItemBase<FilterUpgradeWrapper> {
	public static final UpgradeType<FilterUpgradeWrapper> TYPE = new UpgradeType<>(FilterUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public FilterUpgradeItem(IntSupplier filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<FilterUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
