package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class InceptionBackpackPersistenceTest {
	@Test
	void saveInitializedSubBackpacksResavesNestedBackpackWithContentsUuid() throws ReflectiveOperationException {
		InventoryHandler inventoryHandler = mock(InventoryHandler.class);
		when(inventoryHandler.getSlots()).thenReturn(0);
		ItemStack nestedStack = mock(ItemStack.class);
		when(inventoryHandler.getStackInSlot(0)).thenReturn(nestedStack);
		IBackpackWrapper nestedBackpack = mock(IBackpackWrapper.class);
		when(nestedBackpack.getContentsUuid()).thenReturn(Optional.of(UUID.randomUUID()));
		when(nestedBackpack.getBackpack()).thenReturn(nestedStack);
		SubBackpacksHandler subBackpacksHandler = new SubBackpacksHandler(inventoryHandler);
		getSubBackpacks(subBackpacksHandler).put(0, nestedBackpack);

		subBackpacksHandler.saveInitializedSubBackpacks();

		verify(inventoryHandler).setStackInSlot(0, nestedStack);
		verify(inventoryHandler).saveInventory();
	}

	@Test
	void saveInitializedSubBackpacksPreservesLinkedEndpoint() throws ReflectiveOperationException {
		InventoryHandler inventoryHandler = mock(InventoryHandler.class);
		when(inventoryHandler.getSlots()).thenReturn(0);
		ItemStack endpointStack = mock(ItemStack.class);
		CompoundTag linkedStorage = new CompoundTag();
		when(endpointStack.getOrCreateTagElement(anyString())).thenReturn(linkedStorage);
		when(endpointStack.getTagElement(anyString())).thenReturn(linkedStorage);
		LinkedStorageStackData.setEndpoint(endpointStack, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		when(inventoryHandler.getStackInSlot(0)).thenReturn(endpointStack);
		IBackpackWrapper nestedBackpack = mock(IBackpackWrapper.class);
		when(nestedBackpack.getContentsUuid()).thenReturn(Optional.of(UUID.randomUUID()));
		when(nestedBackpack.getBackpack()).thenReturn(mock(ItemStack.class));
		SubBackpacksHandler subBackpacksHandler = new SubBackpacksHandler(inventoryHandler);
		getSubBackpacks(subBackpacksHandler).put(0, nestedBackpack);

		subBackpacksHandler.saveInitializedSubBackpacks();

		verify(inventoryHandler, never()).setStackInSlot(0, nestedBackpack.getBackpack());
		verify(inventoryHandler, never()).saveInventory();
	}

	@SuppressWarnings("unchecked")
	private static Map<Integer, IStorageWrapper> getSubBackpacks(SubBackpacksHandler subBackpacksHandler) throws ReflectiveOperationException {
		Field subBackpacksField = SubBackpacksHandler.class.getDeclaredField("subBackpacks");
		subBackpacksField.setAccessible(true);
		return (Map<Integer, IStorageWrapper>) subBackpacksField.get(subBackpacksHandler);
	}
}
