package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class InceptionBackpackPersistenceTest {
	@Test
	void saveInitializedSubBackpacksResavesNestedBackpackWithContentsUuid() throws ReflectiveOperationException {
		InventoryHandler inventoryHandler = mock(InventoryHandler.class);
		when(inventoryHandler.getSlots()).thenReturn(0);
		IBackpackWrapper nestedBackpack = mock(IBackpackWrapper.class);
		when(nestedBackpack.getContentsUuid()).thenReturn(Optional.of(UUID.randomUUID()));
		when(nestedBackpack.getBackpack()).thenReturn(null);
		SubBackpacksHandler subBackpacksHandler = new SubBackpacksHandler(inventoryHandler);
		getSubBackpacks(subBackpacksHandler).put(0, nestedBackpack);

		subBackpacksHandler.saveInitializedSubBackpacks();

		verify(inventoryHandler).setStackInSlot(0, null);
		verify(inventoryHandler).saveInventory();
	}

	@SuppressWarnings("unchecked")
	private static Map<Integer, IStorageWrapper> getSubBackpacks(SubBackpacksHandler subBackpacksHandler) throws ReflectiveOperationException {
		Field subBackpacksField = SubBackpacksHandler.class.getDeclaredField("subBackpacks");
		subBackpacksField.setAccessible(true);
		return (Map<Integer, IStorageWrapper>) subBackpacksField.get(subBackpacksHandler);
	}
}
