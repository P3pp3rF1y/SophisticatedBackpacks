package net.p3pp3rf1y.sophisticatedcore.upgrades.feeding;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.function.IntSupplier;

public class FeedingUpgradeItem extends UpgradeItemBase<FeedingUpgradeWrapper> {
	public static final UpgradeType<FeedingUpgradeWrapper> TYPE = new UpgradeType<>(FeedingUpgradeWrapper::new);

	private final IntSupplier filterSlotCount;

	public FeedingUpgradeItem(IntSupplier filterSlotCount, CreativeModeTab itemGroup) {
		super(itemGroup);
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
