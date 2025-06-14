package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;
import java.util.function.IntSupplier;

public class RefillUpgradeItem extends UpgradeItemBase<RefillUpgradeWrapper> {
	private static final UpgradeType<RefillUpgradeWrapper> TYPE = new UpgradeType<>(RefillUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;
	private final boolean targetSlotSelection;
	private final boolean supportsBlockPick;

	public RefillUpgradeItem(IntSupplier filterSlotCount, boolean targetSlotSelection, boolean supportsBlockPick, Properties properties) {
		super(Config.SERVER.maxUpgradesPerStorage, properties);
		this.filterSlotCount = filterSlotCount;
		this.targetSlotSelection = targetSlotSelection;
		this.supportsBlockPick = supportsBlockPick;
	}

	public boolean supportsBlockPick() {
		return supportsBlockPick;
	}

	@Override
	public UpgradeType<RefillUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return List.of();
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}

	public boolean allowsTargetSlotSelection() {
		return targetSlotSelection;
	}
}
