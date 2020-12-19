package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting;

import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class CraftingUpgradeItem extends UpgradeItemBase<CraftingUpgradeWrapper> {
	private static final UpgradeType<CraftingUpgradeWrapper> TYPE = new UpgradeType<>(CraftingUpgradeWrapper::new);

	@Override
	public UpgradeType<CraftingUpgradeWrapper> getType() {
		return TYPE;
	}
}
