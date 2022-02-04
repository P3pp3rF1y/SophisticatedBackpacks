package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeSlotChangeResult;

public interface IUpgradeItem<T extends IUpgradeWrapper> {
	UpgradeType<T> getType();

	default UpgradeSlotChangeResult canAddUpgradeTo(IStorageWrapper storageWrapper, ItemStack upgradeStack, boolean firstLevelStorage) {
		return new UpgradeSlotChangeResult.Success();
	}

	default UpgradeSlotChangeResult canRemoveUpgradeFrom(IStorageWrapper storageWrapper) {
		return new UpgradeSlotChangeResult.Success();
	}

	default UpgradeSlotChangeResult canSwapUpgradeFor(ItemStack upgradeStackToPut, IStorageWrapper storageWrapper) {
		return canRemoveUpgradeFrom(storageWrapper);
	}

	default int getInventoryColumnsTaken() {
		return 0;
	}
}
