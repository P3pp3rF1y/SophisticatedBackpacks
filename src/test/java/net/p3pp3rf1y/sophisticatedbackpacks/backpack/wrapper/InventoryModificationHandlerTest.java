package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class InventoryModificationHandlerTest {
	@Test
	void reentryWhileWrappingUsesBaseInventory() {
		IStorageWrapper storageWrapper = mock(IStorageWrapper.class);
		InventoryHandler inventoryHandler = mock(InventoryHandler.class);
		ITrackedContentsItemHandler wrappedInventoryHandler = mock(ITrackedContentsItemHandler.class);
		UpgradeHandler upgradeHandler = mock(UpgradeHandler.class);
		IInventoryWrapperUpgrade inventoryWrapperUpgrade = mock(IInventoryWrapperUpgrade.class);
		when(storageWrapper.getInventoryHandler()).thenReturn(inventoryHandler);
		when(storageWrapper.getUpgradeHandler()).thenReturn(upgradeHandler);
		when(upgradeHandler.getWrappersThatImplement(IInventoryWrapperUpgrade.class)).thenReturn(List.of(inventoryWrapperUpgrade));

		InventoryModificationHandler modificationHandler = new InventoryModificationHandler(storageWrapper);
		when(inventoryWrapperUpgrade.wrapInventory(inventoryHandler)).thenAnswer(invocation -> {
			assertSame(inventoryHandler, modificationHandler.getModifiedInventoryHandler());
			return wrappedInventoryHandler;
		});

		assertSame(wrappedInventoryHandler, modificationHandler.getModifiedInventoryHandler());
	}
}
