package net.p3pp3rf1y.sophisticatedcore.upgrades.compacting;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.function.IntSupplier;

public class CompactingUpgradeItem extends UpgradeItemBase<CompactingUpgradeWrapper> {
	private static final UpgradeType<CompactingUpgradeWrapper> TYPE = new UpgradeType<>(CompactingUpgradeWrapper::new);
	private final boolean shouldCompactThreeByThree;
	private final IntSupplier filterSlotCount;

	public CompactingUpgradeItem(boolean shouldCompactThreeByThree, IntSupplier filterSlotCount, CreativeModeTab itemGroup) {
		super(itemGroup);
		this.shouldCompactThreeByThree = shouldCompactThreeByThree;
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<CompactingUpgradeWrapper> getType() {
		return TYPE;
	}

	public boolean shouldCompactThreeByThree() {
		return shouldCompactThreeByThree;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
