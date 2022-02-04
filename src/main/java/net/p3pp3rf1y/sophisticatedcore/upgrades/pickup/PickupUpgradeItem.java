package net.p3pp3rf1y.sophisticatedcore.upgrades.pickup;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.function.IntSupplier;

public class PickupUpgradeItem extends UpgradeItemBase<PickupUpgradeWrapper> {
	public static final UpgradeType<PickupUpgradeWrapper> TYPE = new UpgradeType<>(PickupUpgradeWrapper::new);

	private final IntSupplier filterSlotCount;

	public PickupUpgradeItem(IntSupplier filterSlotCount, CreativeModeTab itemGroup) {
		super(itemGroup);
		this.filterSlotCount = filterSlotCount;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}

	@Override
	public UpgradeType<PickupUpgradeWrapper> getType() {
		return TYPE;
	}
}
