package net.p3pp3rf1y.sophisticatedcore.upgrades.filter;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.function.IntSupplier;

public class FilterUpgradeItem extends UpgradeItemBase<FilterUpgradeWrapper> {
	public static final UpgradeType<FilterUpgradeWrapper> TYPE = new UpgradeType<>(FilterUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public FilterUpgradeItem(IntSupplier filterSlotCount, CreativeModeTab itemGroup) {
		super(itemGroup);
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
