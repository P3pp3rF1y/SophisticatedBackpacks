package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;

import java.util.List;

public class InventoryModificationHandler {
	private final IStorageWrapper backpackWrapper;
	private ITrackedContentsItemHandler modifiedInventoryHandler;

	public InventoryModificationHandler(IStorageWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	public ITrackedContentsItemHandler getModifiedInventoryHandler() {
		if (modifiedInventoryHandler == null) {
			initializeWrappedInventory(backpackWrapper.getInventoryHandler());
		}
		return modifiedInventoryHandler;
	}

	private void initializeWrappedInventory(ITrackedContentsItemHandler inventoryHandler) {
		List<IInventoryWrapperUpgrade> inventoryWrapperUpgrades = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IInventoryWrapperUpgrade.class);

		ITrackedContentsItemHandler wrappedHandler = inventoryHandler;
		for (IInventoryWrapperUpgrade inventoryWrapperUpgrade : inventoryWrapperUpgrades) {
			wrappedHandler = inventoryWrapperUpgrade.wrapInventory(wrappedHandler);
		}

		modifiedInventoryHandler = wrappedHandler;
	}
}
