package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.function.IntSupplier;

public class FeedingUpgradeItem extends UpgradeItemBase<FeedingUpgradeWrapper> {
	public static final UpgradeType<FeedingUpgradeWrapper> TYPE = new UpgradeType<>(FeedingUpgradeWrapper::new);

	private final IntSupplier filterSlotCount;

	public FeedingUpgradeItem(IntSupplier filterSlotCount) {
		this.filterSlotCount = filterSlotCount;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}

	@Override
	public UpgradeType<FeedingUpgradeWrapper> getType() {
		return TYPE;
	}
}
