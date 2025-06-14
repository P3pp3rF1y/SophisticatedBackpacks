package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;
import java.util.function.IntSupplier;

public class RestockUpgradeItem extends UpgradeItemBase<RestockUpgradeWrapper> {
	private static final UpgradeType<RestockUpgradeWrapper> TYPE = new UpgradeType<>(RestockUpgradeWrapper::new);
	private final IntSupplier filterSlotCount;

	public RestockUpgradeItem(IntSupplier filterSlotCount, Properties properties) {
		super(Config.SERVER.maxUpgradesPerStorage, properties);
		this.filterSlotCount = filterSlotCount;
	}

	@Override
	public UpgradeType<RestockUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return List.of();
	}

	public int getFilterSlotCount() {
		return filterSlotCount.getAsInt();
	}
}
