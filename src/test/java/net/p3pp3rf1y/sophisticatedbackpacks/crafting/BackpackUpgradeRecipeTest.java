package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.NonNullList;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.CustomData;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.minecraft.world.item.crafting.ShapedRecipePattern;
import net.neoforged.fml.config.IConfigSpec;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.CapturedMob;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherStorage;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayout;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitResult;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitter;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutPart;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.StorageWrapperRepository;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.util.CodecHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BackpackUpgradeRecipeTest {
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
	void prepareInventoryLayoutMarksChangedColumnsWithoutMutatingSource() {
		ItemStack source = new ItemStack(ModItems.GOLD_BACKPACK.get());
		ItemStack result = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		result.applyComponents(source.getComponents());

		try (MockedStatic<BackpackWrapper> wrappers = Mockito.mockStatic(BackpackWrapper.class)) {
			BackpackUpgradeRecipe.prepareInventoryLayout(source, result, ModItems.DIAMOND_BACKPACK.get());

			wrappers.verifyNoInteractions();
		}

		assertFalse(source.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertEquals(9, result.get(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertEquals(ModItems.GOLD_BACKPACK.get().getNumberOfSlots(), result.get(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS));
		assertEquals(ModItems.GOLD_BACKPACK.get().getNumberOfUpgradeSlots(), result.get(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS));
		assertTrue(result.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID));
		assertFalse(result.has(ModCoreDataComponents.STORAGE_UUID));
		assertEquals(ModItems.DIAMOND_BACKPACK.get().getNumberOfSlots(), result.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS));
	}

	@Test
	void prepareInventoryLayoutReadsLegacyInlineGeometryWithoutMigratingSource() {
		ItemStack source = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		CompoundTag legacyData = new CompoundTag();
		legacyData.putInt("inventorySlots", 82);
		legacyData.putInt("columnsTaken", 2);
		source.set(DataComponents.CUSTOM_DATA, CustomData.of(legacyData));
		ItemStack result = new ItemStack(ModItems.GOLD_BACKPACK.get());
		result.applyComponents(source.getComponents());

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, ModItems.GOLD_BACKPACK.get());

		assertEquals(10, result.get(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertFalse(source.has(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS));
		assertFalse(source.has(ModCoreDataComponents.STORAGE_UUID));
		assertFalse(result.has(ModCoreDataComponents.STORAGE_UUID));
	}

	@Test
	void prepareInventoryLayoutUsesNineColumnsForPartialSourceRows() {
		ItemStack source = new ItemStack(ModItems.GOLD_BACKPACK.get());
		source.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 80);
		ItemStack result = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		result.applyComponents(source.getComponents());

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, ModItems.DIAMOND_BACKPACK.get());

		assertEquals(9, result.get(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
	}

	@Test
	void prepareInventoryLayoutUsesTwelveColumnsAtEightyTwoSlots() {
		ItemStack source = new ItemStack(ModItems.GOLD_BACKPACK.get());
		source.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 82);
		ItemStack result = new ItemStack(ModItems.GOLD_BACKPACK.get());
		result.applyComponents(source.getComponents());

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, ModItems.GOLD_BACKPACK.get());

		assertEquals(12, result.get(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
	}

	@Test
	void prepareInventoryLayoutDefersLinkedSourceGeometryToCompletion() {
		ItemStack source = new ItemStack(ModItems.GOLD_BACKPACK.get());
		source.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 80);
		source.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		ItemStack result = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		result.applyComponents(source.getComponents());

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, ModItems.DIAMOND_BACKPACK.get());

		assertFalse(result.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertFalse(result.has(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS));
		assertFalse(result.has(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS));
		assertTrue(result.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID));
	}

	@Test
	void prepareInventoryLayoutSkipsMarkersAndWrapperInteractionWhenColumnsDoNotChange() {
		ItemStack source = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		ItemStack result = new ItemStack(ModItems.NETHERITE_BACKPACK.get());
		result.applyComponents(source.getComponents());

		try (MockedStatic<BackpackWrapper> wrappers = Mockito.mockStatic(BackpackWrapper.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			BackpackUpgradeRecipe.prepareInventoryLayout(source, result, ModItems.NETHERITE_BACKPACK.get());

			wrappers.verifyNoInteractions();
			fitter.verifyNoInteractions();
		}

		assertFalse(result.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertFalse(result.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID));
	}

	@Test
	void completeTierUpgradeMovesConflictingStackAndPreservesCapturedMob() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob capturedMob = capturedMob(17, 2, 2);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMob, 20);

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
			assertEquals(List.of(capturedMob.id()), MobCatcherStorage.getCapturedMobs(fixture.upgradedWrapper()).stream().map(CapturedMob::id).toList());
			assertEquals(1, countStacks(fixture.upgradedWrapper(), Items.DIAMOND), () -> copyInventory(fixture.upgradedWrapper()).toString());
			assertTrue(fixture.upgradedWrapper().getInventoryHandler().getStackInSlot(20).isEmpty());
		});
	}

	@Test
	void completeTierUpgradeRepositionsFourCapturedMobsFromGoldToDiamond() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> capturedMobs = List.of(capturedMob(0, 3, 4), capturedMob(3, 3, 4), capturedMob(6, 3, 4), capturedMob(36, 3, 4));
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMobs, List.of());

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			assertCapturedMobLayout(fixture.upgradedWrapper(), capturedMobs, List.of(0, 3, 6, 48));
			assertEquals(108, fixture.upgradedWrapper().getInventoryHandler().getSlots());
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void completeTierUpgradeRepositionsCapturedMobsWithTwoColumnsTaken() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> capturedMobs = List.of(capturedMob(7, 2, 2), capturedMob(28, 3, 2));
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMobs, List.of(), 2);
			assertEquals(2, fixture.source().get(ModDataComponents.COLUMNS_TAKEN));
			assertEquals(7, InventoryLayout.getEffectiveColumns(ModItems.GOLD_BACKPACK.get().getNumberOfSlots(), fixture.sourceWrapper().getColumnsTaken()));
			assertEquals(63,
					InventoryLayout.getUsableInventorySlots(ModItems.GOLD_BACKPACK.get().getNumberOfSlots(), fixture.sourceWrapper().getColumnsTaken()));
			assertEquals(63, fixture.sourceWrapper().getInventoryHandler().getSlots());
			assertEquals(7, fixture.delivered().get(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			assertEquals(10,
					InventoryLayout.getEffectiveColumns(ModItems.DIAMOND_BACKPACK.get().getNumberOfSlots(), fixture.upgradedWrapper().getColumnsTaken()));
			assertEquals(90,
					InventoryLayout.getUsableInventorySlots(ModItems.DIAMOND_BACKPACK.get().getNumberOfSlots(), fixture.upgradedWrapper().getColumnsTaken()));
			assertCapturedMobLayout(fixture.upgradedWrapper(), capturedMobs, List.of(10, 40), 10);
			assertEquals(90, fixture.upgradedWrapper().getInventoryHandler().getSlots());
			assertEquals(2, fixture.delivered().get(ModDataComponents.COLUMNS_TAKEN));
			assertEquals(2, fixture.upgradedWrapper().getColumnsTaken());
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void completeTierUpgradeMigratesFromColdCache() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> capturedMobs = List.of(capturedMob(0, 3, 4), capturedMob(3, 3, 4), capturedMob(6, 3, 4), capturedMob(36, 3, 4));
			RealWrapperCompletionFixture fixture = createUncachedCompletionFixture(capturedMobs, List.of());
			StorageWrapperRepository.clearCache();

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			IBackpackWrapper completed = BackpackWrapper.fromStack(fixture.delivered());
			assertCapturedMobLayout(completed, capturedMobs, List.of(0, 3, 6, 48));
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void completeTierUpgradeMigratesOnPreResolvedTargetHandler() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> capturedMobs = List.of(capturedMob(0, 3, 4), capturedMob(3, 3, 4), capturedMob(6, 3, 4), capturedMob(36, 3, 4));
			RealWrapperCompletionFixture fixture = createUncachedCompletionFixture(capturedMobs, List.of());
			StorageWrapperRepository.clearCache();
			IBackpackWrapper target = BackpackWrapper.fromStack(fixture.delivered());
			assertEquals(108, target.getInventoryHandler().getSlots());

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			assertCapturedMobLayout(target, capturedMobs, List.of(0, 3, 6, 48));
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void createDetachedInventoryLayoutViewCopiesCanonicalContentsWithoutMutatingStacks() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob capturedMob = capturedMob(0, 3, 4);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMob, 20);
			ItemStack sourceBefore = fixture.source().copy();
			ItemStack deliveredBefore = fixture.delivered().copy();
			UUID contentsUuid = fixture.upgradedWrapper().getContentsUuid().orElseThrow();
			CompoundTag contentsBefore = BackpackStorage.get().getBackpackContents(contentsUuid).orElseThrow().copy();
			assertTrue(fixture.upgradedWrapper().getUpgradeHandler().getStackInSlot(0).is(ModItems.MOB_CATCHER_UPGRADE.get()));
			assertTrue(contentsBefore.contains("upgradeInventory"));

			BackpackWrapper detached = BackpackWrapper.createDetachedInventoryLayoutView(fixture.delivered(), 81, 4);

			assertTrue(detached.getContentsUuid().isEmpty());
			assertEquals(4, detached.getUpgradeHandler().getSlots());
			assertTrue(detached.getUpgradeHandler().getStackInSlot(0).is(ModItems.MOB_CATCHER_UPGRADE.get()));
			assertEquals(List.of(capturedMob), MobCatcherStorage.getCapturedMobs(detached));
			assertTrue(detached.getInventoryLayoutParts(9, 12).stream().anyMatch(part -> part.id().startsWith("mob:")));
			assertTrue(ItemStack.matches(sourceBefore, fixture.source()));
			assertTrue(ItemStack.matches(deliveredBefore, fixture.delivered()));
			assertEquals(contentsBefore, BackpackStorage.get().getBackpackContents(contentsUuid).orElseThrow());
		});
	}

	@Test
	void createDetachedInventoryLayoutViewReadsLegacyCanonicalContentsWithoutMutatingStacks() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob capturedMob = capturedMob(0, 3, 4);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMob, 20);
			UUID contentsUuid = fixture.upgradedWrapper().getContentsUuid().orElseThrow();
			ItemStack source = new ItemStack(ModItems.GOLD_BACKPACK.get());
			CompoundTag legacyData = new CompoundTag();
			legacyData.putUUID("contentsUuid", contentsUuid);
			source.set(DataComponents.CUSTOM_DATA, CustomData.of(legacyData));
			ItemStack result = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
			result.applyComponents(source.getComponents());
			ItemStack sourceBefore = source.copy();
			ItemStack resultBefore = result.copy();
			CompoundTag contentsBefore = BackpackStorage.get().getBackpackContents(contentsUuid).orElseThrow().copy();
			long cacheChanges = StorageWrapperRepository.getWrapperCacheChangeCounter();

			BackpackWrapper detached = BackpackWrapper.createDetachedInventoryLayoutView(result, 81, 4);

			assertTrue(detached.getContentsUuid().isEmpty());
			assertTrue(detached.getUpgradeHandler().getStackInSlot(0).is(ModItems.MOB_CATCHER_UPGRADE.get()));
			assertEquals(List.of(capturedMob), MobCatcherStorage.getCapturedMobs(detached));
			assertTrue(detached.getInventoryLayoutParts(9, 12).stream().anyMatch(part -> part.id().startsWith("mob:")));
			assertTrue(ItemStack.matches(sourceBefore, source));
			assertTrue(ItemStack.matches(resultBefore, result));
			assertEquals(contentsBefore, BackpackStorage.get().getBackpackContents(contentsUuid).orElseThrow());
			assertEquals(cacheChanges, StorageWrapperRepository.getWrapperCacheChangeCounter());
		});
	}

	@Test
	void createDetachedInventoryLayoutViewUsesRecordedHighestUpgradeSlot() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob capturedMob = capturedMob(0, 3, 4);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMob, 20, 2);

			BackpackWrapper detached = BackpackWrapper.createDetachedInventoryLayoutView(fixture.delivered(), 81, 3);

			assertTrue(detached.getUpgradeHandler().getStackInSlot(2).is(ModItems.MOB_CATCHER_UPGRADE.get()));
			assertTrue(detached.getInventoryLayoutParts(9, 12).stream().anyMatch(part -> part.id().startsWith("mob:")));
		});
	}

	@Test
	void completeTierUpgradeRepositionsCapturedMobsAcrossGoldToDiamondLayouts() throws Throwable {
		List<CapturedMobLayoutCase> cases = List.of(new CapturedMobLayoutCase(List.of(new CapturedMobLayout(0, 2, 2, 0), new CapturedMobLayout(3, 3, 1, 3),
				new CapturedMobLayout(17, 1, 3, 20), new CapturedMobLayout(36, 3, 4, 48))),
				new CapturedMobLayoutCase(List.of(new CapturedMobLayout(60, 3, 3, 78))));

		for (CapturedMobLayoutCase testCase : cases) {
			runOnServerThread(() -> {
				List<CapturedMob> capturedMobs = testCase.layouts().stream().map(layout -> capturedMob(layout.sourceAnchor(), layout.width(), layout.height()))
						.toList();
				RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMobs, List.of());

				BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

				assertCapturedMobLayout(fixture.upgradedWrapper(), capturedMobs, testCase.layouts().stream().map(CapturedMobLayout::targetAnchor).toList());
				assertEquals(108, fixture.upgradedWrapper().getInventoryHandler().getSlots());
				assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
			});
		}
	}

	@Test
	void completeTierUpgradeDoesNotMutateInventoryOrCapturedMobsWhenFitFails() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob capturedMob = capturedMob(0, 13, 1);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMob, 20);
			IBackpackWrapper upgradedWrapper = fixture.upgradedWrapper();
			assertEquals(81, upgradedWrapper.getInventoryHandler().getSlots());
			List<ItemStack> inventoryBefore = copyInventory(upgradedWrapper);
			List<CapturedMob> capturedMobsBefore = MobCatcherStorage.getCapturedMobs(upgradedWrapper);

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			assertInventoryEquals(inventoryBefore, upgradedWrapper);
			assertEquals(capturedMobsBefore, MobCatcherStorage.getCapturedMobs(upgradedWrapper));
			assertEquals(81, upgradedWrapper.getInventoryHandler().getSlots());
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void completeTierUpgradeCompletesLinkedUpgradeBeforeFittingCanonicalTarget() {
		LinkedCompletionFixture fixture = createLinkedCompletionFixture();
		Container inputs = Mockito.mock(Container.class);
		LinkedLayoutSetup setup = createLinkedLayoutSetup();
		AtomicInteger resolves = new AtomicInteger();

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class);
				MockedStatic<BackpackLinkedStorageEndpointAdapter> adapter = Mockito.mockStatic(BackpackLinkedStorageEndpointAdapter.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			stubLinkedTierUpgradeCompletion(fixture, inputs, setup, resolves, resolver, adapter, fitter);

			BackpackUpgradeRecipe.completeTierUpgrade(fixture.level(), fixture.player(), fixture.callback(), inputs);
		}

		assertEquals(List.of("resolve source", "snapshot source", "fit", "adapter", "resolve target", "apply target"), setup.events());
		Mockito.verify(setup.source()).getInventoryLayoutParts(9, 12);
		Mockito.verify(setup.target(), Mockito.never()).getInventoryLayoutParts(Mockito.anyInt(), Mockito.anyInt());
		Mockito.verify(setup.source(), Mockito.never()).applyInventoryLayout(Mockito.any(), Mockito.anyInt());
		Mockito.verify(setup.target()).applyInventoryLayout(setup.fitResult(), 12);
		assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
	}

	@Test
	void completeTierUpgradeStopsLinkedCompletionWhenFitFails() {
		LinkedCompletionFixture fixture = createLinkedCompletionFixture();
		Container inputs = Mockito.mock(Container.class);
		LinkedLayoutSetup setup = createLinkedLayoutSetup();
		AtomicInteger resolves = new AtomicInteger();
		Mockito.when(setup.fitResult().fits()).thenReturn(false);

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class);
				MockedStatic<BackpackLinkedStorageEndpointAdapter> adapter = Mockito.mockStatic(BackpackLinkedStorageEndpointAdapter.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			stubLinkedTierUpgradeCompletion(fixture, inputs, setup, resolves, resolver, adapter, fitter);

			BackpackUpgradeRecipe.completeTierUpgrade(fixture.level(), fixture.player(), fixture.callback(), inputs);

			resolver.verify(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(fixture.level(), fixture.delivered()));
			adapter.verifyNoInteractions();
		}

		assertEquals(List.of("resolve source", "snapshot source", "fit"), setup.events());
		Mockito.verify(setup.source()).getInventoryLayoutParts(9, 12);
		Mockito.verify(setup.source(), Mockito.never()).applyInventoryLayout(Mockito.any(), Mockito.anyInt());
		Mockito.verify(setup.target(), Mockito.never()).applyInventoryLayout(Mockito.any(), Mockito.anyInt());
		assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
	}

	@Test
	void completeTierUpgradeStopsLinkedCompletionWhenAdapterFails() {
		LinkedCompletionFixture fixture = createLinkedCompletionFixture();
		IBackpackWrapper source = layoutWrapper(81);
		InventoryHandler sourceHandler = source.getInventoryHandler();
		Mockito.when(sourceHandler.getSlots()).thenReturn(81);
		Mockito.when(source.getInventoryLayoutParts(9, 12)).thenReturn(List.of(new InventoryLayoutPart("mob:source", 0, 1, 1, Set.of(0))));
		InventoryLayoutFitResult fitResult = Mockito.mock(InventoryLayoutFitResult.class);
		Mockito.when(fitResult.fits()).thenReturn(true);

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class);
				MockedStatic<BackpackLinkedStorageEndpointAdapter> adapter = Mockito.mockStatic(BackpackLinkedStorageEndpointAdapter.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(fixture.level(), fixture.delivered()))
					.thenReturn(Optional.of(source));
			adapter.when(() -> BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(fixture.level(), fixture.delivered())).thenReturn(false);
			fitter.when(() -> InventoryLayoutFitter.fit(Mockito.anyList(), Mockito.eq(108), Mockito.eq(12))).thenReturn(fitResult);

			BackpackUpgradeRecipe.completeTierUpgrade(fixture.level(), fixture.player(), fixture.callback(), null);

			resolver.verify(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(fixture.level(), fixture.delivered()));
			adapter.verify(() -> BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(fixture.level(), fixture.delivered()));
			fitter.verify(() -> InventoryLayoutFitter.fit(Mockito.anyList(), Mockito.eq(108), Mockito.eq(12)));
		}

		Mockito.verify(source).getInventoryLayoutParts(9, 12);
		Mockito.verify(source, Mockito.never()).applyInventoryLayout(Mockito.any(), Mockito.anyInt());
		assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
	}

	@Test
	void completeTierUpgradeRemovesMarkersWhenLinkedSourceIsAbsent() {
		LinkedCompletionFixture fixture = createLinkedCompletionFixture();

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class);
				MockedStatic<BackpackLinkedStorageEndpointAdapter> adapter = Mockito.mockStatic(BackpackLinkedStorageEndpointAdapter.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(fixture.level(), fixture.delivered())).thenReturn(Optional.empty());

			BackpackUpgradeRecipe.completeTierUpgrade(fixture.level(), fixture.player(), fixture.callback(), null);

			resolver.verify(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(fixture.level(), fixture.delivered()));
			adapter.verifyNoInteractions();
			fitter.verifyNoInteractions();
		}

		assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
	}

	@Test
	void completeTierUpgradeLeavesMarkersWhenWrapperResolutionThrows() {
		ItemStack delivered = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		delivered.set(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS, 9);
		delivered.set(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS, 81);
		delivered.set(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS, 4);
		delivered.set(ModDataComponents.TIER_UPGRADE_OPERATION_ID, UUID.randomUUID());
		ItemStack callback = delivered.copy();
		Player player = playerWithDeliveredStack(delivered);

		try (MockedStatic<BackpackWrapper> wrappers = Mockito.mockStatic(BackpackWrapper.class)) {
			wrappers.when(() -> BackpackWrapper.createDetachedInventoryLayoutView(delivered, 81, 4)).thenThrow(new IllegalStateException("unexpected"));

			assertThrows(IllegalStateException.class, () -> BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), player, callback, null));
		}

		assertTierUpgradeMarkersRetained(delivered, callback);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(CapturedMob capturedMob, int occupiedSlot) {
		return createRealWrapperCompletionFixture(capturedMob, occupiedSlot, 0);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(CapturedMob capturedMob, int occupiedSlot, int upgradeSlot) {
		return createRealWrapperCompletionFixture(List.of(capturedMob), List.of(new OccupiedStack(occupiedSlot, new ItemStack(Items.DIAMOND))), upgradeSlot,
				true);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks) {
		return createRealWrapperCompletionFixture(capturedMobs, occupiedStacks, 0, true);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks,
			int columnsTaken) {
		return createRealWrapperCompletionFixture(capturedMobs, occupiedStacks, 0, true, columnsTaken);
	}

	private RealWrapperCompletionFixture createUncachedCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks) {
		return createRealWrapperCompletionFixture(capturedMobs, occupiedStacks, 0, false);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks, int upgradeSlot,
			boolean cacheDeliveredAsSource) {
		return createRealWrapperCompletionFixture(capturedMobs, occupiedStacks, upgradeSlot, cacheDeliveredAsSource, 0);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks, int upgradeSlot,
			boolean cacheDeliveredAsSource, int columnsTaken) {
		ItemStack source = new ItemStack(ModItems.GOLD_BACKPACK.get());
		if (columnsTaken != 0) {
			source.set(ModDataComponents.COLUMNS_TAKEN, columnsTaken);
		}
		IBackpackWrapper sourceWrapper = BackpackWrapper.fromStack(source);
		sourceWrapper.setPersistent(true);
		sourceWrapper.getInventoryHandler();
		storageUuids.add(sourceWrapper.getContentsUuid().orElseThrow());
		sourceWrapper.getUpgradeHandler().setStackInSlot(upgradeSlot, new ItemStack(ModItems.MOB_CATCHER_UPGRADE.get()));
		sourceWrapper.getBackpackContents().orElseThrow().put("upgradeInventory",
				sourceWrapper.getUpgradeHandler().serializeNBT(RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY)));
		capturedMobs.forEach(capturedMob -> MobCatcherStorage.addCapturedMob(sourceWrapper, capturedMob));
		occupiedStacks.forEach(occupiedStack -> sourceWrapper.getInventoryHandler().setSlotStack(occupiedStack.slot(), occupiedStack.stack()));
		persistInventory(sourceWrapper);
		ItemStack delivered = upgradeRecipe(ModItems.GOLD_BACKPACK.get(), ModItems.DIAMOND_BACKPACK.get()).assemble(CraftingInput.of(1, 1, List.of(source)),
				RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY));
		IBackpackWrapper upgradedWrapper = null;
		if (cacheDeliveredAsSource) {
			StorageWrapperRepository.setStorageWrapper(delivered, sourceWrapper);
			upgradedWrapper = BackpackWrapper.fromStack(delivered);
		}
		ItemStack callback = delivered.copy();
		return new RealWrapperCompletionFixture(source, delivered, callback, playerWithDeliveredStack(delivered), sourceWrapper, upgradedWrapper);
	}

	private static LinkedCompletionFixture createLinkedCompletionFixture() {
		ItemStack delivered = linkedDeliveredStack();
		return new LinkedCompletionFixture(delivered, delivered.copy(), playerWithDeliveredStack(delivered), Mockito.mock(ServerLevel.class));
	}

	private static LinkedLayoutSetup createLinkedLayoutSetup() {
		IBackpackWrapper source = layoutWrapper(81);
		IBackpackWrapper target = layoutWrapper(108);
		InventoryLayoutFitResult fitResult = Mockito.mock(InventoryLayoutFitResult.class);
		List<InventoryLayoutPart> parts = List.of(new InventoryLayoutPart("mob:source", 0, 1, 1, Set.of(0)));
		List<String> events = new ArrayList<>();
		Mockito.when(source.getInventoryLayoutParts(9, 12)).thenAnswer(invocation -> {
			events.add("snapshot source");
			return parts;
		});
		Mockito.when(fitResult.fits()).thenReturn(true);
		Mockito.doAnswer(invocation -> {
			events.add("apply target");
			return null;
		}).when(target).applyInventoryLayout(fitResult, 12);
		return new LinkedLayoutSetup(source, target, fitResult, parts, events);
	}

	private static void stubLinkedTierUpgradeCompletion(LinkedCompletionFixture fixture, Container inputs, LinkedLayoutSetup setup, AtomicInteger resolves,
			MockedStatic<BackpackLinkedStorageResolver> resolver, MockedStatic<BackpackLinkedStorageEndpointAdapter> adapter,
			MockedStatic<InventoryLayoutFitter> fitter) {
		resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(fixture.level(), fixture.delivered())).thenAnswer(invocation -> {
			boolean source = resolves.getAndIncrement() == 0;
			setup.events().add(source ? "resolve source" : "resolve target");
			return Optional.of(source ? setup.source() : setup.target());
		});
		adapter.when(() -> BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(fixture.level(), fixture.delivered(), inputs))
				.thenAnswer(invocation -> {
					setup.events().add("adapter");
					return true;
				});
		fitter.when(() -> InventoryLayoutFitter.fit(setup.parts(), 108, 12)).thenAnswer(invocation -> {
			setup.events().add("fit");
			return setup.fitResult();
		});
	}

	private static void assertCapturedMobLayout(IBackpackWrapper wrapper, List<CapturedMob> sourceMobs, List<Integer> expectedAnchors) {
		assertCapturedMobLayout(wrapper, sourceMobs, expectedAnchors, 12);
	}

	private static void assertCapturedMobLayout(IBackpackWrapper wrapper, List<CapturedMob> sourceMobs, List<Integer> expectedAnchors, int columns) {
		List<CapturedMob> actualMobs = MobCatcherStorage.getCapturedMobs(wrapper);
		assertEquals(sourceMobs.size(), actualMobs.size());
		assertEquals(expectedAnchors, actualMobs.stream().map(CapturedMob::slot).toList());

		int slots = wrapper.getInventoryHandler().getSlots();
		Set<Integer> occupiedSlots = new HashSet<>();
		for (int mobIndex = 0; mobIndex < actualMobs.size(); mobIndex++) {
			CapturedMob sourceMob = sourceMobs.get(mobIndex);
			CapturedMob actualMob = actualMobs.get(mobIndex);
			assertEquals(sourceMob.id(), actualMob.id());
			assertEquals(sourceMob.width(), actualMob.width());
			assertEquals(sourceMob.height(), actualMob.height());
			assertTrue(actualMob.slot() % columns + actualMob.width() <= columns);
			for (int y = 0; y < actualMob.height(); y++) {
				for (int x = 0; x < actualMob.width(); x++) {
					int occupiedSlot = actualMob.slot() + y * columns + x;
					assertTrue(occupiedSlot >= 0 && occupiedSlot < slots);
					assertEquals(actualMob.slot() / columns + y, occupiedSlot / columns);
					assertTrue(occupiedSlots.add(occupiedSlot));
				}
			}
		}
	}

	private static void assertTierUpgradeMarkersRemoved(ItemStack delivered, ItemStack callback) {
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS));
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS));
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID));
		assertFalse(callback.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertFalse(callback.has(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS));
		assertFalse(callback.has(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS));
		assertFalse(callback.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID));
	}

	private static void assertTierUpgradeMarkersRetained(ItemStack delivered, ItemStack callback) {
		assertTrue(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertTrue(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS));
		assertTrue(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS));
		assertTrue(delivered.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID));
		assertTrue(callback.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS));
		assertTrue(callback.has(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS));
		assertTrue(callback.has(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS));
		assertTrue(callback.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID));
	}

	private record RealWrapperCompletionFixture(ItemStack source, ItemStack delivered, ItemStack callback, Player player, IBackpackWrapper sourceWrapper,
			IBackpackWrapper upgradedWrapper) {
	}

	private record OccupiedStack(int slot, ItemStack stack) {
	}

	private record LinkedCompletionFixture(ItemStack delivered, ItemStack callback, Player player, ServerLevel level) {
	}

	private record LinkedLayoutSetup(IBackpackWrapper source, IBackpackWrapper target, InventoryLayoutFitResult fitResult, List<InventoryLayoutPart> parts,
			List<String> events) {
	}

	private record CapturedMobLayoutCase(List<CapturedMobLayout> layouts) {
	}

	private record CapturedMobLayout(int sourceAnchor, int width, int height, int targetAnchor) {
	}

	private static BackpackUpgradeRecipe upgradeRecipe(BackpackItem source, BackpackItem result) {
		return new BackpackUpgradeRecipe(new ShapedRecipe("", CraftingBookCategory.MISC,
				new ShapedRecipePattern(1, 1, NonNullList.of(Ingredient.EMPTY, Ingredient.of(source)), Optional.empty()), new ItemStack(result)));
	}

	private static Player playerWithDeliveredStack(ItemStack delivered) {
		Player player = Mockito.mock(Player.class);
		Inventory inventory = new Inventory(player);
		inventory.items.set(0, delivered);
		AbstractContainerMenu menu = Mockito.mock(AbstractContainerMenu.class);
		player.containerMenu = menu;
		Mockito.when(menu.getCarried()).thenReturn(ItemStack.EMPTY);
		Mockito.when(player.getInventory()).thenReturn(inventory);
		return player;
	}

	private static ItemStack linkedDeliveredStack() {
		ItemStack delivered = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		delivered.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		delivered.set(ModDataComponents.TIER_UPGRADE_OPERATION_ID, UUID.randomUUID());
		return delivered;
	}

	private static IBackpackWrapper layoutWrapper(int slots) {
		IBackpackWrapper wrapper = Mockito.mock(IBackpackWrapper.class);
		InventoryHandler inventoryHandler = Mockito.mock(InventoryHandler.class);
		Mockito.when(wrapper.getInventoryHandler()).thenReturn(inventoryHandler);
		Mockito.when(wrapper.getColumnsTaken()).thenReturn(0);
		Mockito.when(wrapper.getNumberOfSlotRows()).thenReturn(9);
		Mockito.when(inventoryHandler.getSlots()).thenReturn(slots);
		return wrapper;
	}

	private static CapturedMob capturedMob(int slot, int width, int height) {
		return new CapturedMob(UUID.randomUUID(), ResourceLocation.withDefaultNamespace("pig"), new CompoundTag(), slot, width, height, width * height, false,
				"Pig", 10, 10);
	}

	private static int countStacks(IBackpackWrapper wrapper, Item item) {
		int count = 0;
		for (int slot = 0; slot < wrapper.getInventoryHandler().getSlots(); slot++) {
			if (wrapper.getInventoryHandler().getStackInSlot(slot).is(item)) {
				count++;
			}
		}
		return count;
	}

	private static List<ItemStack> copyInventory(IBackpackWrapper wrapper) {
		List<ItemStack> stacks = new ArrayList<>();
		for (int slot = 0; slot < wrapper.getInventoryHandler().getSlots(); slot++) {
			stacks.add(wrapper.getInventoryHandler().getStackInSlot(slot).copy());
		}
		return stacks;
	}

	private static void persistInventory(IBackpackWrapper wrapper) {
		RegistryAccess registries = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
		ListTag items = new ListTag();
		for (int slot = 0; slot < wrapper.getInventoryHandler().getSlots(); slot++) {
			ItemStack stack = wrapper.getInventoryHandler().getStackInSlot(slot);
			if (!stack.isEmpty()) {
				CompoundTag item = new CompoundTag();
				item.putInt("Slot", slot);
				items.add(CodecHelper.OVERSIZED_ITEM_STACK_CODEC.encode(stack, registries.createSerializationContext(NbtOps.INSTANCE), item).getOrThrow());
			}
		}
		CompoundTag inventory = new CompoundTag();
		inventory.put("Items", items);
		inventory.putInt("Size", wrapper.getInventoryHandler().getSlots());
		wrapper.getBackpackContents().orElseThrow().put(InventoryHandler.INVENTORY_TAG, inventory);
	}

	private static void assertInventoryEquals(List<ItemStack> expected, IBackpackWrapper wrapper) {
		for (int slot = 0; slot < expected.size(); slot++) {
			assertTrue(ItemStack.matches(expected.get(slot), wrapper.getInventoryHandler().getStackInSlot(slot)));
		}
	}

	private static void loadDefaultConfig(ModConfigSpec spec) throws ReflectiveOperationException {
		if (spec.isLoaded()) {
			return;
		}
		CommentedConfig config = CommentedConfig.inMemory();
		spec.correct(config);
		Class<?> loadedClass = Class.forName("net.neoforged.fml.config.LoadedConfig");
		Constructor<?> constructor = loadedClass.getDeclaredConstructor(CommentedConfig.class, Path.class, ModConfig.class);
		constructor.setAccessible(true);
		spec.acceptConfig((IConfigSpec.ILoadedConfig) constructor.newInstance(config, null, null));
	}

	private static void runOnServerThread(Runnable action) throws Throwable {
		AtomicReference<Throwable> failure = new AtomicReference<>();
		Thread thread = new Thread(SidedThreadGroups.SERVER, () -> {
			try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registryHelper.when(RegistryHelper::getRegistryAccess)
						.thenReturn(Optional.of(RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY)));
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
