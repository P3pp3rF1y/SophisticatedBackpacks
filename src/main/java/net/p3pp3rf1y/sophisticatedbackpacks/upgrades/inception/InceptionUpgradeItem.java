package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

public class InceptionUpgradeItem extends UpgradeItemBase<InceptionUpgradeWrapper> {
	public static final UpgradeType<InceptionUpgradeWrapper> TYPE = new UpgradeType<>(InceptionUpgradeWrapper::new);

	@Override
	public UpgradeType<InceptionUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public boolean canAddUpgradeTo(IBackpackWrapper backpackWrapper, boolean firstLevelBackpack) {
		return firstLevelBackpack && !backpackWrapper.getUpgradeHandler().hasUpgrade(TYPE);
	}

	@Override
	public boolean canRemoveUpgradeFrom(IBackpackWrapper backpackWrapper) {
		return !InventoryHelper.hasItem(backpackWrapper.getInventoryHandler(), stack -> stack.getItem() instanceof BackpackItem);
	}
}
