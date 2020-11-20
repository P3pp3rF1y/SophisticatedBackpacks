package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ItemBase;

public abstract class UpgradeItemBase<T extends IUpgradeWrapper> extends ItemBase implements IBackpackUpgradeItem<T> {
	public UpgradeItemBase() {
		super(new Properties().maxStackSize(1));
	}
}
