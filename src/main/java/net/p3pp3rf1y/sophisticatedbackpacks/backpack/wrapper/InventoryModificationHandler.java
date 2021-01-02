package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;

import java.util.List;

public class InventoryModificationHandler {
	private final IBackpackWrapper backpackWrapper;
	private IItemHandlerModifiable modifiedInventoryHandler;

	public InventoryModificationHandler(IBackpackWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	public IItemHandlerModifiable getModifiedInventoryHandler() {
		if (modifiedInventoryHandler == null) {
			initializeWrappedInventory(backpackWrapper.getInventoryHandler());
		}
		return modifiedInventoryHandler;
	}

	private void initializeWrappedInventory(IItemHandlerModifiable inventoryHandler) {
		List<IInventoryWrapperUpgrade> inventoryWrapperUpgrades = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IInventoryWrapperUpgrade.class);

		IItemHandlerModifiable wrappedHandler = inventoryHandler;
		for (IInventoryWrapperUpgrade inventoryWrapperUpgrade : inventoryWrapperUpgrades) {
			wrappedHandler = inventoryWrapperUpgrade.wrapInventory(wrappedHandler);
		}

		modifiedInventoryHandler = new InsertResponseInventoryWrapper(backpackWrapper, wrappedHandler);
	}
}
