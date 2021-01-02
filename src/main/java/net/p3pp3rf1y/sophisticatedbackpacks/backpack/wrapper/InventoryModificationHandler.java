package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IObservableItemHandler;

import java.util.List;

public class InventoryModificationHandler {
	private final IBackpackWrapper backpackWrapper;
	private IItemHandlerModifiable modifiedInventoryHandler;

	public InventoryModificationHandler(IBackpackWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	public IItemHandlerModifiable getModifiedInventoryHandler() {
		if (modifiedInventoryHandler == null) {
			backpackWrapper.getInventoryHandler().clearListeners();
			initializeWrappedInventory(backpackWrapper.getInventoryHandler());
		}
		return modifiedInventoryHandler;
	}

	private void initializeWrappedInventory(IObservableItemHandler inventoryHandler) {
		List<IInventoryWrapperUpgrade> inventoryWrapperUpgrades = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IInventoryWrapperUpgrade.class);

		IObservableItemHandler wrappedHandler = inventoryHandler;
		for (IInventoryWrapperUpgrade inventoryWrapperUpgrade : inventoryWrapperUpgrades) {
			wrappedHandler = inventoryWrapperUpgrade.wrapInventory(wrappedHandler);
		}

		modifiedInventoryHandler = new InsertResponseInventoryWrapper(backpackWrapper, wrappedHandler);
	}
}
