package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemResourceHandler;
import org.jspecify.annotations.Nullable;

import java.util.List;

public class InventoryModificationHandler {
	private final IStorageWrapper backpackWrapper;
	@Nullable
	private ITrackedContentsItemResourceHandler modifiedInventoryHandler;

	public InventoryModificationHandler(IStorageWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	public ITrackedContentsItemResourceHandler getModifiedInventoryHandler() {
		if (modifiedInventoryHandler == null) {
			ITrackedContentsItemResourceHandler inventoryHandler = backpackWrapper.getInventoryHandler();
			modifiedInventoryHandler = inventoryHandler;
			initializeWrappedInventory(inventoryHandler);
		}
		return modifiedInventoryHandler;
	}

	private void initializeWrappedInventory(ITrackedContentsItemResourceHandler inventoryHandler) {
		List<IInventoryWrapperUpgrade> inventoryWrapperUpgrades = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IInventoryWrapperUpgrade.class);

		ITrackedContentsItemResourceHandler wrappedHandler = inventoryHandler;
		for (IInventoryWrapperUpgrade inventoryWrapperUpgrade : inventoryWrapperUpgrades) {
			wrappedHandler = inventoryWrapperUpgrade.wrapInventory(wrappedHandler);
		}

		modifiedInventoryHandler = wrappedHandler;
	}
}
