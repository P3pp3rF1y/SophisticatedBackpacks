package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IItemHandlerSimpleInserter;

import java.util.List;

public class InventoryModificationHandler {
	private final IBackpackWrapper backpackWrapper;
	private IItemHandlerSimpleInserter modifiedInventoryHandler;

	public InventoryModificationHandler(IBackpackWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	public IItemHandlerSimpleInserter getModifiedInventoryHandler() {
		if (modifiedInventoryHandler == null) {
			initializeWrappedInventory(backpackWrapper.getInventoryHandler());
		}
		return modifiedInventoryHandler;
	}

	private void initializeWrappedInventory(IItemHandlerSimpleInserter inventoryHandler) {
		List<IInventoryWrapperUpgrade> inventoryWrapperUpgrades = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IInventoryWrapperUpgrade.class);

		IItemHandlerSimpleInserter wrappedHandler = inventoryHandler;
		for (IInventoryWrapperUpgrade inventoryWrapperUpgrade : inventoryWrapperUpgrades) {
			wrappedHandler = inventoryWrapperUpgrade.wrapInventory(wrappedHandler);
		}

		modifiedInventoryHandler = wrappedHandler;
	}
}
