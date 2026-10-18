package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluids;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.event.tick.LevelTickEvent;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplates;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.UUIDDeduplicator;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonEventHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.StorageWrapperRepository;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.*;

class BackpackLegacyRegressionTest {
	private static final RegistryAccess REGISTRY_ACCESS = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
	private final Set<UUID> storageUuids = new HashSet<>();

	@BeforeAll
	static void setup() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		loadDefaultConfig(Config.SERVER_SPEC);
		Constructor<RegisterCapabilitiesEvent> constructor = RegisterCapabilitiesEvent.class.getDeclaredConstructor();
		constructor.setAccessible(true);
		Method registerCapabilities = ModItems.class.getDeclaredMethod("registerCapabilities", RegisterCapabilitiesEvent.class);
		registerCapabilities.setAccessible(true);
		registerCapabilities.invoke(null, constructor.newInstance());
	}

	@AfterEach
	void cleanup() {
		StorageWrapperRepository.clearCache();
		storageUuids.forEach(BackpackStorage.get()::removeBackpackContents);
	}

	@Test
	void ordinaryCopiedStackKeepsIndependentPhysicalWrapper() throws Throwable {
		runOnServerThread(() -> {
			ItemStack original = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(original);
			wrapper.getInventoryHandler();
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			ItemStack copy = original.copy();

			IBackpackWrapper copiedWrapper = BackpackWrapper.fromStack(copy);

			assertNotSame(wrapper, copiedWrapper);
			assertSame(original, wrapper.getBackpack());
			assertSame(copy, copiedWrapper.getBackpack());
		});
	}

	@Test
	void setSlotNumbersRefreshesInitializedHandlers() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			InventoryHandler inventoryHandler = wrapper.getInventoryHandler();
			UpgradeHandler upgradeHandler = wrapper.getUpgradeHandler();
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());

			wrapper.setSlotNumbers(1, 1);

			assertNotSame(inventoryHandler, wrapper.getInventoryHandler());
			assertNotSame(upgradeHandler, wrapper.getUpgradeHandler());
			assertEquals(1, wrapper.getInventoryHandler().getSlots());
			assertEquals(1, wrapper.getUpgradeHandler().getSlots());
		});
	}

	@Test
	void fillFromTemplateRefreshesInitializedHandlers() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			InventoryHandler inventoryHandler = wrapper.getInventoryHandler();
			UpgradeHandler upgradeHandler = wrapper.getUpgradeHandler();
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			ResourceLocation templateName = ResourceLocation.fromNamespaceAndPath("test", "replacement");
			CompoundTag template = new CompoundTag();
			template.put("backpackContents", new CompoundTag());

			try (MockedStatic<BackpackTemplates> templates = Mockito.mockStatic(BackpackTemplates.class)) {
				templates.when(() -> BackpackTemplates.getBackpackTemplate(templateName)).thenReturn(Optional.of(template));
				wrapper.setTemplate(templateName);
				wrapper.fillFromTemplate();
			}

			assertNotSame(inventoryHandler, wrapper.getInventoryHandler());
			assertNotSame(upgradeHandler, wrapper.getUpgradeHandler());
		});
	}

	@Test
	void setContentsUuidRefreshesInitializedHandlersOnlyWhenReassigned() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			InventoryHandler inventoryHandler = wrapper.getInventoryHandler();
			UpgradeHandler upgradeHandler = wrapper.getUpgradeHandler();
			UUID originalUuid = wrapper.getContentsUuid().orElseThrow();
			UUID replacementUuid = UUID.randomUUID();
			storageUuids.add(originalUuid);
			storageUuids.add(replacementUuid);

			wrapper.setContentsUuid(originalUuid);
			assertSame(inventoryHandler, wrapper.getInventoryHandler());
			assertSame(upgradeHandler, wrapper.getUpgradeHandler());
			wrapper.setContentsUuid(replacementUuid);

			assertNotSame(inventoryHandler, wrapper.getInventoryHandler());
			assertNotSame(upgradeHandler, wrapper.getUpgradeHandler());
		});
	}

	@Test
	void removeContentsUuidEvictsCanonicalFluidOwnerAndRefreshesInitializedHandlers() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registries = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registries.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
				IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
				wrapper.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
				InventoryHandler inventoryHandler = wrapper.getInventoryHandler();
				wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
				UUID storageUuid = wrapper.getContentsUuid().orElseThrow();
				storageUuids.add(storageUuid);
				ItemStack copy = stack.copy();
				IBackpackWrapper copiedWrapper = BackpackWrapper.fromStack(copy);
				copiedWrapper.getUpgradeHandler();
				assertTrue(BackpackWrapper.getCapabilityFluidHandler(copy).isPresent());

				wrapper.removeContentsUuid();

				assertFalse(stack.has(ModCoreDataComponents.STORAGE_UUID));
				assertNotSame(inventoryHandler, wrapper.getInventoryHandler());
				storageUuids.add(wrapper.getContentsUuid().orElseThrow());
				assertTrue(BackpackWrapper.getCapabilityFluidHandler(copy).isEmpty());
			}
		});
	}

	@Test
	void copiedWrapperUpgradeMutationRefreshesCanonicalFluidOwner() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registries = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registries.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				ItemStack original = new ItemStack(ModItems.BACKPACK.get());
				IBackpackWrapper owner = BackpackWrapper.fromStack(original);
				owner.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
				owner.getInventoryHandler();
				owner.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
				storageUuids.add(owner.getContentsUuid().orElseThrow());
				assertTrue(BackpackWrapper.getCapabilityFluidHandler(original).isPresent());
				ItemStack copy = original.copy();
				IBackpackWrapper copiedWrapper = BackpackWrapper.fromStack(copy);

				copiedWrapper.getUpgradeHandler().setStackInSlot(0, ItemStack.EMPTY);

				assertTrue(BackpackWrapper.getCapabilityFluidHandler(original).isEmpty());
				copiedWrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));

				assertTrue(BackpackWrapper.getCapabilityFluidHandler(original).isPresent());
			}
		});
	}

	@Test
	void copiedWrapperInventoryMutationRefreshesCanonicalFluidOwner() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registries = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registries.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				ItemStack original = new ItemStack(ModItems.BACKPACK.get());
				IBackpackWrapper owner = BackpackWrapper.fromStack(original);
				owner.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
				owner.getInventoryHandler();
				owner.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
				storageUuids.add(owner.getContentsUuid().orElseThrow());
				IBackpackWrapper child = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
				child.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
				child.getInventoryHandler();
				child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
				storageUuids.add(child.getContentsUuid().orElseThrow());
				ItemStack childStack = child.getBackpack();
				owner.getInventoryHandler().setStackInSlot(0, childStack);
				assertEquals(1, BackpackWrapper.getCapabilityFluidHandler(original).orElseThrow().getTanks());
				IBackpackWrapper copiedWrapper = BackpackWrapper.fromStack(original.copy());

				copiedWrapper.getInventoryHandler().setStackInSlot(0, ItemStack.EMPTY);

				assertEquals(0, BackpackWrapper.getCapabilityFluidHandler(original).orElseThrow().getTanks());
				copiedWrapper.getInventoryHandler().setStackInSlot(0, childStack);

				assertEquals(1, BackpackWrapper.getCapabilityFluidHandler(original).orElseThrow().getTanks());
			}
		});
	}

	@Test
	void failedFluidUtilProbesKeepPhysicalOwnerAndCacheCardinality() throws Throwable {
		runOnServerThread(() -> {
			ItemStack original = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(original);
			wrapper.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
			wrapper.getInventoryHandler();
			wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			assertNotNull(FluidUtil.getFluidHandler(original).orElse(null));
			long changes = StorageWrapperRepository.getWrapperCacheChangeCounter();
			FluidTank empty = new FluidTank(1000);

			for (int index = 0; index < 50; index++) {
				assertFalse(FluidUtil.tryFillContainer(original, empty, 1000, null, true).isSuccess());
				assertFalse(FluidUtil.tryEmptyContainer(original, empty, 1000, null, false).isSuccess());
			}

			assertSame(original, wrapper.getBackpack());
			assertSame(wrapper, BackpackWrapper.fromStack(original));
			assertEquals(changes, StorageWrapperRepository.getWrapperCacheChangeCounter());
		});
	}

	@Test
	void successfulFluidUtilTransferReturnsUpdatedCopyWithoutStealingPhysicalOwner() throws Throwable {
		runOnServerThread(() -> {
			ItemStack original = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(original);
			wrapper.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
			wrapper.getInventoryHandler();
			wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			assertTrue(FluidUtil.getFluidHandler(original).isPresent());
			FluidTank source = new FluidTank(1000);
			source.setFluid(new FluidStack(Fluids.WATER, 1000));

			var filled = FluidUtil.tryFillContainer(original, source, 1000, null, true);

			assertTrue(filled.isSuccess());
			assertNotSame(original, filled.getResult());
			assertEquals(wrapper.getContentsUuid().orElseThrow(), filled.getResult().get(ModCoreDataComponents.STORAGE_UUID));
			assertSame(original, wrapper.getBackpack());
			assertTrue(source.getFluid().isEmpty());
			assertEquals(1000, FluidUtil.getFluidHandler(filled.getResult()).orElseThrow().getFluidInTank(0).getAmount());
			assertFalse(BackpackRenderInfo.fromPhysicalStack(filled.getResult()).getTankRenderInfos().isEmpty());
		});
	}

	@Test
	void simulatedFluidUtilTransferReturnsCopyAndRestoresPhysicalOwner() throws Throwable {
		runOnServerThread(() -> {
			ItemStack original = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(original);
			wrapper.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
			wrapper.getInventoryHandler();
			wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			assertTrue(FluidUtil.getFluidHandler(original).isPresent());
			FluidTank source = new FluidTank(1000);
			source.setFluid(new FluidStack(Fluids.WATER, 1000));

			var simulated = FluidUtil.tryFillContainer(original, source, 1000, null, false);

			assertTrue(simulated.isSuccess());
			assertNotSame(original, simulated.getResult());
			assertSame(original, wrapper.getBackpack());
			assertEquals(1000, source.getFluid().getAmount());
			assertEquals(1000, FluidUtil.getFluidHandler(simulated.getResult()).orElseThrow().getFluidInTank(0).getAmount());
		});
	}

	@Test
	void physicalDedupeRetainsLargestOfThree() throws Throwable {
		runOnServerThread(() -> {
			ItemStack first = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper firstWrapper = BackpackWrapper.fromStack(first);
			firstWrapper.getInventoryHandler();
			UUID uuid = firstWrapper.getContentsUuid().orElseThrow();
			storageUuids.add(uuid);
			ItemStack middle = first.copy();
			middle.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 36);
			ItemStack largest = first.copy();
			largest.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 45);
			Player player = Mockito.mock(Player.class);
			PlayerInventoryProvider provider = Mockito.mock(PlayerInventoryProvider.class);
			try (MockedStatic<PlayerInventoryProvider> providers = Mockito.mockStatic(PlayerInventoryProvider.class)) {
				providers.when(PlayerInventoryProvider::get).thenReturn(provider);
				Mockito.doAnswer(invocation -> {
					PlayerInventoryProvider.BackpackInventorySlotConsumer consumer = invocation.getArgument(1);
					consumer.accept(first, PlayerInventoryProvider.MAIN_INVENTORY, "", 0);
					consumer.accept(middle, PlayerInventoryProvider.MAIN_INVENTORY, "", 1);
					consumer.accept(largest, PlayerInventoryProvider.MAIN_INVENTORY, "", 2);
					return false;
				}).when(provider).runOnBackpacks(Mockito.eq(player), Mockito.any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));

				UUIDDeduplicator.checkForDuplicateBackpacksAndRemoveTheirUUID(player, uuid, first);
			}

			assertFalse(first.has(ModCoreDataComponents.STORAGE_UUID));
			assertFalse(middle.has(ModCoreDataComponents.STORAGE_UUID));
			assertEquals(uuid, largest.get(ModCoreDataComponents.STORAGE_UUID));
			assertSame(largest, BackpackWrapper.fromStack(largest).getBackpack());
		});
	}

	@Test
	void physicalDedupeNeverRemovesLinkedEndpointIdentity() throws Throwable {
		runOnServerThread(() -> {
			ItemStack ordinary = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(ordinary);
			wrapper.getInventoryHandler();
			UUID uuid = wrapper.getContentsUuid().orElseThrow();
			storageUuids.add(uuid);
			ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
			endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(uuid, UUID.randomUUID()));
			Player player = Mockito.mock(Player.class);
			PlayerInventoryProvider provider = Mockito.mock(PlayerInventoryProvider.class);
			try (MockedStatic<PlayerInventoryProvider> providers = Mockito.mockStatic(PlayerInventoryProvider.class)) {
				providers.when(PlayerInventoryProvider::get).thenReturn(provider);
				Mockito.doAnswer(invocation -> {
					PlayerInventoryProvider.BackpackInventorySlotConsumer consumer = invocation.getArgument(1);
					consumer.accept(ordinary, PlayerInventoryProvider.MAIN_INVENTORY, "", 0);
					consumer.accept(endpoint, PlayerInventoryProvider.MAIN_INVENTORY, "", 1);
					return false;
				}).when(provider).runOnBackpacks(Mockito.eq(player), Mockito.any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));

				UUIDDeduplicator.checkForDuplicateBackpacksAndRemoveTheirUUID(player, uuid, ordinary);
			}

			assertEquals(uuid, ordinary.get(ModCoreDataComponents.STORAGE_UUID));
			assertTrue(endpoint.has(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT));
		});
	}

	@Test
	void onWorldTickSkipsStaleLinkedEndpointBeforeResolvingPhysicalWrapper() throws Throwable {
		runOnServerThread(() -> {
			UUID storageUuid = UUID.randomUUID();
			ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
			endpoint.set(ModCoreDataComponents.STORAGE_UUID, storageUuid);
			endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
			ServerPlayer player = Mockito.mock(ServerPlayer.class);
			ServerLevel level = Mockito.mock(ServerLevel.class);
			Mockito.when(level.dimension()).thenReturn(Level.OVERWORLD);
			Mockito.when(level.players()).thenReturn(java.util.List.of(player));
			PlayerInventoryProvider provider = Mockito.mock(PlayerInventoryProvider.class);

			try (MockedStatic<PlayerInventoryProvider> providers = Mockito.mockStatic(PlayerInventoryProvider.class);
					MockedStatic<BackpackWrapper> wrappers = Mockito.mockStatic(BackpackWrapper.class)) {
				providers.when(PlayerInventoryProvider::get).thenReturn(provider);
				Mockito.doAnswer(invocation -> {
					PlayerInventoryProvider.BackpackInventorySlotConsumer consumer = invocation.getArgument(1);
					consumer.accept(endpoint, PlayerInventoryProvider.MAIN_INVENTORY, "", 0);
					return false;
				}).when(provider).runOnBackpacks(Mockito.eq(player), Mockito.any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));

				invokeOnWorldTick(new CommonEventHandler(), new LevelTickEvent.Post(() -> true, level));

				wrappers.verifyNoInteractions();
			}

			assertEquals(storageUuid, endpoint.get(ModCoreDataComponents.STORAGE_UUID));
			assertTrue(endpoint.has(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT));
		});
	}

	@Test
	void craftedBackpackCompletesLinkedPrimaryTierUpgrade() {
		ItemStack crafted = new ItemStack(ModItems.GOLD_BACKPACK.get());
		ServerLevel level = Mockito.mock(ServerLevel.class);
		Player player = Mockito.mock(Player.class);
		Mockito.when(player.level()).thenReturn(level);

		try (MockedStatic<BackpackLinkedStorageEndpointAdapter> adapter = Mockito.mockStatic(BackpackLinkedStorageEndpointAdapter.class)) {
			ModItems.GOLD_BACKPACK.get().onCraftedBy(crafted, level, player);

			adapter.verify(() -> BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(level, crafted));
		}
	}

	@Test
	void retainedInceptionEnergyHandlerRefreshesAfterChildUpgradeChanges() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registries = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registries.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				IBackpackWrapper parent = createBackpack(1);
				IBackpackWrapper child = createBackpack(1);
				parent.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
				parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
				assertTrue(parent.getEnergyStorage().isEmpty());

				child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
				var aggregate = parent.getEnergyStorage().orElseThrow();
				int capacity = aggregate.getMaxEnergyStored();
				child.getUpgradeHandler().getSlotWrappers().get(0).setEnabled(false);
				assertTrue(parent.getEnergyStorage().isEmpty());
				child.getUpgradeHandler().getSlotWrappers().get(0).setEnabled(true);

				assertTrue(capacity > 0);
				assertSame(aggregate, parent.getEnergyStorage().orElseThrow());
				assertEquals(capacity, aggregate.getMaxEnergyStored());
			}
		});
	}

	@Test
	void nestedInceptionFluidCompositionDoesNotDuplicateChildTanks() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registries = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registries.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				IBackpackWrapper parent = createBackpack(1);
				IBackpackWrapper child = createBackpack(1);
				child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
				parent.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
				parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());

				int childTanks = child.getFluidHandler().orElseThrow().getTanks();
				int parentTanks = parent.getFluidHandler().orElseThrow().getTanks();

				assertTrue(childTanks > 0);
				assertEquals(childTanks, parentTanks);
			}
		});
	}

	@Test
	void removingOneInceptionParentPreservesOtherParentsChildSubscription() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registries = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registries.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				IBackpackWrapper first = createBackpack(1);
				IBackpackWrapper second = createBackpack(1);
				IBackpackWrapper child = createBackpack(1);
				first.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
				second.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
				child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
				first.getInventoryHandler().setStackInSlot(0, child.getBackpack());
				second.getInventoryHandler().setStackInSlot(0, child.getBackpack());
				assertTrue(first.getEnergyStorage().isPresent());
				assertTrue(second.getEnergyStorage().isPresent());
				assertEquals(2, callbackCount(child));

				second.getInventoryHandler().setStackInSlot(0, ItemStack.EMPTY);
				child.getUpgradeHandler().getSlotWrappers().get(0).setEnabled(false);

				assertEquals(1, callbackCount(child));
				assertTrue(first.getEnergyStorage().isEmpty());
			}
		});
	}

	@Test
	void togglingLocalEnergyUpgradesRebuildsGraphWithoutAccumulatingChildCallbacks() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registries = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registries.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				int baseSlots = ModItems.BACKPACK.get().getNumberOfSlots();
				IBackpackWrapper parent = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
				parent.setSlotNumbers(baseSlots, 2);
				parent.getInventoryHandler();
				storageUuids.add(parent.getContentsUuid().orElseThrow());
				IBackpackWrapper child = createBackpack(baseSlots);
				assertNotEquals(parent.getContentsUuid(), child.getContentsUuid());
				child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
				parent.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
				parent.getUpgradeHandler().setStackInSlot(1, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
				parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
				var battery = parent.getUpgradeHandler().getSlotWrappers().get(0);
				int childCapacity = child.getEnergyStorage().orElseThrow().getMaxEnergyStored();
				var originalAggregate = parent.getEnergyStorage().orElseThrow();
				int combinedCapacity = originalAggregate.getMaxEnergyStored();
				assertTrue(combinedCapacity > childCapacity);
				assertNotEquals(parent.getContentsUuid(), child.getContentsUuid());
				assertEquals(child.getContentsUuid().orElseThrow(), child.getBackpack().get(ModCoreDataComponents.STORAGE_UUID));
				assertEquals(childCapacity, child.getEnergyStorage().orElseThrow().getMaxEnergyStored());

				for (int index = 0; index < 5; index++) {
					battery.setEnabled(false);
					assertFalse(battery.isEnabled());
					assertTrue(
							parent.getUpgradeHandler().getWrappersThatImplementFromMainStorage(net.neoforged.neoforge.energy.IEnergyStorage.class).isEmpty());
					assertNotSame(originalAggregate, parent.getEnergyStorage().orElseThrow());
					assertEquals(childCapacity, child.getEnergyStorage().orElseThrow().getMaxEnergyStored());
					assertEquals(childCapacity, parent.getEnergyStorage().orElseThrow().getMaxEnergyStored());
					battery.setEnabled(true);
					assertEquals(combinedCapacity, parent.getEnergyStorage().orElseThrow().getMaxEnergyStored());
					assertEquals(1, callbackCount(child));
				}
			}
		});
	}

	private static int callbackCount(IBackpackWrapper wrapper) {
		try {
			Field field = UpgradeHandler.class.getDeclaredField("refreshCallbacks");
			field.setAccessible(true);
			return ((Set<?>) field.get(wrapper.getUpgradeHandler())).size();
		} catch (ReflectiveOperationException exception) {
			throw new AssertionError(exception);
		}
	}

	private static void invokeOnWorldTick(CommonEventHandler handler, LevelTickEvent.Post event) {
		try {
			Method onWorldTick = CommonEventHandler.class.getDeclaredMethod("onWorldTick", LevelTickEvent.Post.class);
			onWorldTick.setAccessible(true);
			onWorldTick.invoke(handler, event);
		} catch (ReflectiveOperationException exception) {
			throw new AssertionError(exception);
		}
	}

	private IBackpackWrapper createBackpack(int slots) {
		IBackpackWrapper wrapper = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
		wrapper.setSlotNumbers(slots, 1);
		wrapper.getInventoryHandler();
		storageUuids.add(wrapper.getContentsUuid().orElseThrow());
		return wrapper;
	}

	private static void loadDefaultConfig(net.neoforged.neoforge.common.ModConfigSpec spec) throws ReflectiveOperationException {
		if (spec.isLoaded()) {
			return;
		}
		CommentedConfig config = CommentedConfig.inMemory();
		spec.correct(config);
		Class<?> loadedClass = Class.forName("net.neoforged.fml.config.LoadedConfig");
		Constructor<?> constructor = loadedClass.getDeclaredConstructor(CommentedConfig.class, Path.class, net.neoforged.fml.config.ModConfig.class);
		constructor.setAccessible(true);
		spec.acceptConfig((net.neoforged.fml.config.IConfigSpec.ILoadedConfig) constructor.newInstance(config, null, null));
	}

	private static void runOnServerThread(Runnable action) throws Throwable {
		AtomicReference<Throwable> failure = new AtomicReference<>();
		Thread thread = new Thread(net.neoforged.fml.util.thread.SidedThreadGroups.SERVER, () -> {
			try {
				action.run();
			} catch (Throwable throwable) {
				failure.set(throwable);
			}
		});
		thread.start();
		thread.join();
		if (failure.get() != null) {
			throw failure.get();
		}
	}
}
