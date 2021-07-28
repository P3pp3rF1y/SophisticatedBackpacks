package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;

public interface IBackpackUpgradeItem<T extends IUpgradeWrapper> {
	UpgradeType<T> getType();

	default UpgradeSlotChangeResult canAddUpgradeTo(IBackpackWrapper backpackWrapper, ItemStack upgradeStack, boolean firstLevelBackpack) {
		return new UpgradeSlotChangeResult.Success();
	}

	default UpgradeSlotChangeResult canRemoveUpgradeFrom(IBackpackWrapper backpackWrapper) {
		return new UpgradeSlotChangeResult.Success();
	}

	default UpgradeSlotChangeResult canSwapUpgradeFor(ItemStack upgradeStackToPut, IBackpackWrapper backpackWrapper) {
		return canRemoveUpgradeFrom(backpackWrapper);
	}

	default int getInventoryColumnsTaken() {
		return 0;
	}
}
