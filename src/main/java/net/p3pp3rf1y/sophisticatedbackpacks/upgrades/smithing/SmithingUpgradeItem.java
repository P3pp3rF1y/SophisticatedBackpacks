package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;

public class SmithingUpgradeItem extends UpgradeItemBase<SmithingUpgradeWrapper> {
	private static final UpgradeType<SmithingUpgradeWrapper> TYPE = new UpgradeType<>(SmithingUpgradeWrapper::new);
	public SmithingUpgradeItem() {
		super(Config.SERVER.maxUpgradesPerStorage);
	}

	@Override
	public UpgradeType<SmithingUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return List.of();
	}
}
