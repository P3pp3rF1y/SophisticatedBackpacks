package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.network.ItemStackReference;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class BackpackLinkedStorageEndpointAccessProviderTest {
	private final BackpackLinkedStorageEndpointAccessProvider accessProvider = new BackpackLinkedStorageEndpointAccessProvider();
	private ServerPlayer player;
	private PlayerInventoryProvider inventories;
	private PlayerInventoryHandler handler;
	private ItemStack stack;
	private ItemStackReference reference;

	@BeforeAll
	static void bootstrap() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@BeforeEach
	void prepareEquipment() {
		player = mock(ServerPlayer.class);
		inventories = mock(PlayerInventoryProvider.class);
		handler = mock(PlayerInventoryHandler.class);
		stack = mock(ItemStack.class);
		when(stack.getItem()).thenReturn(mock(BackpackItem.class));
		when(inventories.getPlayerInventoryHandler("equipment")).thenReturn(Optional.of(handler));
		when(handler.getIdentifiers(player)).thenReturn(Set.of("back"));
		when(handler.getSlotCount(player, "back")).thenReturn(1);
		when(handler.getStackInSlot(player, "back", 0)).thenReturn(stack);
		reference = new ItemStackReference(ItemStackReference.EQUIPMENT_HANDLER, 0, "equipment", "back");
	}

	@Test
	void resolveEndpointStackReturnsActualEquipmentBackpack() {
		try (MockedStatic<PlayerInventoryProvider> providers = mockStatic(PlayerInventoryProvider.class)) {
			providers.when(PlayerInventoryProvider::get).thenReturn(inventories);

			ItemStack resolved = accessProvider.resolveEndpointStack(player, reference);

			assertSame(stack, resolved);
		}
	}

	@Test
	void resolveEndpointStackRejectsUnknownHandler() {
		reference = new ItemStackReference(ItemStackReference.EQUIPMENT_HANDLER, 0, "unknown", "back");
		try (MockedStatic<PlayerInventoryProvider> providers = mockStatic(PlayerInventoryProvider.class)) {
			providers.when(PlayerInventoryProvider::get).thenReturn(inventories);

			ItemStack resolved = accessProvider.resolveEndpointStack(player, reference);

			assertTrue(resolved.isEmpty());
			verifyNoInteractions(handler);
		}
	}

	@Test
	void resolveEndpointStackRejectsUnknownIdentifierBeforeReadingSlot() {
		reference = new ItemStackReference(ItemStackReference.EQUIPMENT_HANDLER, 0, "equipment", "other");
		try (MockedStatic<PlayerInventoryProvider> providers = mockStatic(PlayerInventoryProvider.class)) {
			providers.when(PlayerInventoryProvider::get).thenReturn(inventories);

			ItemStack resolved = accessProvider.resolveEndpointStack(player, reference);

			assertTrue(resolved.isEmpty());
			verify(handler, never()).getStackInSlot(any(), anyString(), anyInt());
		}
	}

	@Test
	void resolveEndpointStackRejectsNegativeSlotBeforeReadingSlot() {
		reference = new ItemStackReference(ItemStackReference.EQUIPMENT_HANDLER, -1, "equipment", "back");
		try (MockedStatic<PlayerInventoryProvider> providers = mockStatic(PlayerInventoryProvider.class)) {
			providers.when(PlayerInventoryProvider::get).thenReturn(inventories);

			ItemStack resolved = accessProvider.resolveEndpointStack(player, reference);

			assertTrue(resolved.isEmpty());
			verify(handler, never()).getStackInSlot(any(), anyString(), anyInt());
		}
	}

	@Test
	void resolveEndpointStackRejectsOutOfBoundsSlotBeforeReadingSlot() {
		reference = new ItemStackReference(ItemStackReference.EQUIPMENT_HANDLER, 1, "equipment", "back");
		try (MockedStatic<PlayerInventoryProvider> providers = mockStatic(PlayerInventoryProvider.class)) {
			providers.when(PlayerInventoryProvider::get).thenReturn(inventories);

			ItemStack resolved = accessProvider.resolveEndpointStack(player, reference);

			assertTrue(resolved.isEmpty());
			verify(handler, never()).getStackInSlot(any(), anyString(), anyInt());
		}
	}

	@Test
	void resolveEndpointStackRejectsNonBackpackEquipmentItem() {
		when(handler.getStackInSlot(player, "back", 0)).thenReturn(ItemStack.EMPTY);
		try (MockedStatic<PlayerInventoryProvider> providers = mockStatic(PlayerInventoryProvider.class)) {
			providers.when(PlayerInventoryProvider::get).thenReturn(inventories);

			ItemStack resolved = accessProvider.resolveEndpointStack(player, reference);

			assertTrue(resolved.isEmpty());
		}
	}
}
