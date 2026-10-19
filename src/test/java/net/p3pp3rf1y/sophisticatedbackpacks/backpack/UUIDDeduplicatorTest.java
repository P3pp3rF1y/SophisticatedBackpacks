package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.fml.LogicalSide;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonEventHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.*;

class UUIDDeduplicatorTest {
	private static BackpackItem testBackpack;

	@BeforeAll
	static void bootstrapRegistriesAndConfig() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		for (Class<?> type = BuiltInRegistries.ITEM.getClass(); type != null; type = type.getSuperclass()) {
			for (String name : List.of("frozen", "locked")) {
				try {
					Field guard = type.getDeclaredField(name);
					guard.setAccessible(true);
					guard.setBoolean(BuiltInRegistries.ITEM, false);
				} catch (NoSuchFieldException ignored) {
					// Forge wraps the vanilla registry.
				}
			}
		}
		testBackpack = new BackpackItem(() -> 9, () -> 1, () -> null);
		Registry.register(BuiltInRegistries.ITEM, "sophisticatedbackpacks:uuid_dedupe_test_backpack", testBackpack);
		for (Class<?> type = BuiltInRegistries.ITEM.getClass(); type != null; type = type.getSuperclass()) {
			for (String name : List.of("frozen", "locked")) {
				try {
					Field guard = type.getDeclaredField(name);
					guard.setAccessible(true);
					guard.setBoolean(BuiltInRegistries.ITEM, true);
				} catch (NoSuchFieldException ignored) {
					// Forge wraps the vanilla registry.
				}
			}
		}
		if (!Config.SERVER_SPEC.isLoaded()) {
			CommentedConfig config = CommentedConfig.inMemory();
			Config.SERVER_SPEC.correct(config);
			Config.SERVER_SPEC.acceptConfig(config);
		}
	}

	@Test
	void playerInventoryScanSkipsRawLinkedEndpointBeforeResolvingCapability() {
		ItemStack endpoint = linkedStack();
		ItemStack ordinary = spy(new ItemStack(testBackpack));
		doReturn(LazyOptional.of(() -> mock(IBackpackWrapper.class))).when(ordinary).getCapability(any());
		PlayerInventoryProvider provider = mock(PlayerInventoryProvider.class);
		Player player = mock(Player.class);
		doAnswer(invocation -> {
			invocation.getArgument(1, PlayerInventoryProvider.BackpackInventorySlotConsumer.class).accept(endpoint, "main", "", 0);
			return null;
		}).when(provider).runOnBackpacks(eq(player), any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));

		try (MockedStatic<CapabilityManager> capabilities = Mockito.mockStatic(CapabilityManager.class);
				MockedStatic<PlayerInventoryProvider> providers = Mockito.mockStatic(PlayerInventoryProvider.class)) {
			capabilities.when(() -> CapabilityManager.get(any())).thenReturn(mock(Capability.class));
			providers.when(PlayerInventoryProvider::get).thenReturn(provider);
			UUIDDeduplicator.checkForDuplicateBackpacksAndRemoveTheirUUID(player, UUID.randomUUID(), ordinary);
		}

		verify(endpoint, never()).getCapability(any());
	}

	@Test
	void nearbyItemEntityScanSkipsRawLinkedEndpointBeforeResolvingCapability() {
		ItemStack endpoint = linkedStack();
		ItemEntity entity = mock(ItemEntity.class);
		when(entity.getItem()).thenReturn(endpoint);

		UUIDDeduplicator.dedupeBackpackItemEntityInArea(entity);

		verify(endpoint, never()).getCapability(any());
		verify(entity, never()).level();
	}

	@Test
	void periodicWorldTickScanSkipsRawLinkedEndpointBeforeResolvingCapability() throws ReflectiveOperationException {
		ItemStack endpoint = linkedStack();
		PlayerInventoryProvider provider = mock(PlayerInventoryProvider.class);
		ServerPlayer player = mock(ServerPlayer.class);
		ServerLevel level = mock(ServerLevel.class);
		TickEvent.LevelTickEvent tick = new TickEvent.LevelTickEvent(LogicalSide.SERVER, TickEvent.Phase.END, level, () -> true);
		when(level.dimension()).thenReturn(net.minecraft.world.level.Level.OVERWORLD);
		when(level.players()).thenReturn(List.of(player));
		doAnswer(invocation -> {
			invocation.getArgument(1, PlayerInventoryProvider.BackpackInventorySlotConsumer.class).accept(endpoint, "main", "", 0);
			return null;
		}).when(provider).runOnBackpacks(eq(player), any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));
		Method worldTick = CommonEventHandler.class.getDeclaredMethod("onWorldTick", TickEvent.LevelTickEvent.class);
		worldTick.setAccessible(true);

		try (MockedStatic<PlayerInventoryProvider> providers = Mockito.mockStatic(PlayerInventoryProvider.class)) {
			providers.when(PlayerInventoryProvider::get).thenReturn(provider);
			worldTick.invoke(new CommonEventHandler(), tick);
		}

		verify(endpoint, never()).getCapability(any());
		verify(provider).runOnBackpacks(eq(player), any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));
	}

	private static ItemStack linkedStack() {
		ItemStack stack = spy(new ItemStack(testBackpack));
		LinkedStorageStackData.setEndpoint(stack, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		return stack;
	}
	@Test
	void dedupeBackpackWrappersCarriesLargestWinnerAcrossThreePhysicalStacks() {
		IBackpackWrapper small = backpackWithSlots(9);
		IBackpackWrapper large = backpackWithSlots(27);
		IBackpackWrapper medium = backpackWithSlots(18);

		IBackpackWrapper winner = UUIDDeduplicator.dedupeBackpackWrappers(small, large);
		winner = UUIDDeduplicator.dedupeBackpackWrappers(winner, medium);

		assertSame(large, winner);
		verify(small).removeContentsUUIDTag();
		verify(medium).removeContentsUUIDTag();
		verify(large, never()).removeContentsUUIDTag();
	}

	@Test
	void dedupeBackpackWrappersNeverRemovesUuidFromLinkedFacadeOrPhysicalPeer() {
		IBackpackWrapper linked = backpackWithSlots(9);
		IBackpackWrapper ordinary = backpackWithSlots(27);
		when(linked.getLinkedStorageEndpoint()).thenReturn(Optional.of(new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID())));

		assertSame(ordinary, UUIDDeduplicator.dedupeBackpackWrappers(linked, ordinary));
		assertSame(ordinary, UUIDDeduplicator.dedupeBackpackWrappers(ordinary, linked));
		IBackpackWrapper otherLinked = backpackWithSlots(36);
		when(otherLinked.getLinkedStorageEndpoint()).thenReturn(Optional.of(new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID())));
		assertSame(linked, UUIDDeduplicator.dedupeBackpackWrappers(linked, otherLinked));
		assertSame(otherLinked, UUIDDeduplicator.dedupeBackpackWrappers(otherLinked, linked));

		verify(linked, never()).removeContentsUUIDTag();
		verify(ordinary, never()).removeContentsUUIDTag();
		verify(otherLinked, never()).removeContentsUUIDTag();
	}

	private static IBackpackWrapper backpackWithSlots(int slots) {
		IBackpackWrapper backpack = mock(IBackpackWrapper.class);
		InventoryHandler inventory = mock(InventoryHandler.class);
		when(backpack.getInventoryHandler()).thenReturn(inventory);
		when(inventory.getSlots()).thenReturn(slots);
		return backpack;
	}
}
