package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;

public class AnvilUpgradeItem extends UpgradeItemBase<AnvilUpgradeWrapper> {
	private static final UpgradeType<AnvilUpgradeWrapper> TYPE = new UpgradeType<>(AnvilUpgradeWrapper::new);
	public AnvilUpgradeItem(Properties properties) {
		super(Config.SERVER.maxUpgradesPerStorage, properties);
	}

	@Override
	public UpgradeType<AnvilUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return List.of();
	}
}
