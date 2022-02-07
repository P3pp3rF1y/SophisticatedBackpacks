package net.p3pp3rf1y.sophisticatedcore.upgrades.voiding;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.function.IntSupplier;

public class VoidUpgradeItem extends UpgradeItemBase<VoidUpgradeWrapper> {
	public static final UpgradeType<VoidUpgradeWrapper> TYPE = new UpgradeType<>(VoidUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public VoidUpgradeItem(IntSupplier filterSlotCount, CreativeModeTab itemGroup) {
		super(itemGroup);
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<VoidUpgradeWrapper> getType() {
		return TYPE;
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
