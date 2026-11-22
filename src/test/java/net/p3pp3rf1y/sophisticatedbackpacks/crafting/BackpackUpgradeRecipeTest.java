package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.NonNullList;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.inventory.TransientCraftingContainer;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.ListenerList;
import net.minecraftforge.eventbus.api.EventListenerHelper;
import net.minecraftforge.fml.util.thread.SidedThreadGroups;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.CapturedMob;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayout;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitResult;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitter;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutPart;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeCountLimitConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Field;
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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BackpackUpgradeRecipeTest {
	private static BackpackItem GOLD;
	private static BackpackItem DIAMOND;
	private static BackpackItem NETHERITE;
	private static MobCatcherUpgradeItem MOB_CATCHER;
	private final Set<UUID> storageUuids = new HashSet<>();

	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		if (!Config.SERVER_SPEC.isLoaded()) {
			CommentedConfig config = CommentedConfig.inMemory();
			Config.SERVER_SPEC.correct(config);
			Config.SERVER_SPEC.acceptConfig(config);
		}
		setItemRegistryFrozen(false);
		ResourceLocation basicId = new ResourceLocation("sophisticatedbackpacks:backpack");
		if (!BuiltInRegistries.ITEM.containsKey(basicId)) {
			registerBackpack("backpack", 36, 4);
		}
		GOLD = registerBackpack("recipe_gold", 81, 6);
		DIAMOND = registerBackpack("recipe_diamond", 108, 8);
		NETHERITE = registerBackpack("recipe_netherite", 108, 10);
		MOB_CATCHER = Registry.register(BuiltInRegistries.ITEM, "sophisticatedbackpacks:recipe_mob_catcher",
				new MobCatcherUpgradeItem(false, Mockito.mock(IUpgradeCountLimitConfig.class)));
		setItemRegistryFrozen(true);
		// Forge's untransformed JVM test Clone event has no public no-arg constructor.
		try (MockedStatic<EventListenerHelper> listeners = Mockito.mockStatic(EventListenerHelper.class, Mockito.CALLS_REAL_METHODS)) {
			listeners.when(() -> EventListenerHelper.getListenerList(PlayerEvent.Clone.class)).thenReturn(new ListenerList());
			try {
				Class.forName(BackpackMainSettingsCategory.class.getName(), true, BackpackMainSettingsCategory.class.getClassLoader());
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException(e);
			}
		}
	}

	@AfterEach
	void cleanup() {
		storageUuids.forEach(BackpackStorage.get()::removeBackpackContents);
	}

	@Test
	void prepareInventoryLayoutMarksChangedColumnsWithoutMutatingSource() {
		ItemStack source = new ItemStack(GOLD);
		ItemStack result = new ItemStack(DIAMOND);

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, DIAMOND);

		assertNull(ModDataComponents.getSourceColumns(source));
		assertEquals(9, ModDataComponents.getSourceColumns(result));
		assertEquals(GOLD.getNumberOfSlots(), ModDataComponents.getSourceInventorySlots(result));
		assertEquals(GOLD.getNumberOfUpgradeSlots(), ModDataComponents.getSourceUpgradeSlots(result));
		assertTrue(ModDataComponents.getOperationId(result) != null);
		assertFalse(source.hasTag());
		assertFalse(result.getTag().hasUUID(BackpackWrapper.CONTENTS_UUID_TAG));
		assertEquals(DIAMOND.getNumberOfSlots(), result.getTag().getInt("inventorySlots"));
	}

	@Test
	void prepareInventoryLayoutReadsLegacyInlineGeometryWithoutMigratingSource() {
		ItemStack source = new ItemStack(GOLD);
		source.getOrCreateTag().putInt("inventorySlots", 82);
		source.getOrCreateTag().putInt("columnsTaken", 2);
		ItemStack result = new ItemStack(GOLD);
		result.setTag(source.getTag().copy());

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, GOLD);

		assertEquals(10, ModDataComponents.getSourceColumns(result));
		assertEquals(82, ModDataComponents.getSourceInventorySlots(result));
		assertEquals(GOLD.getNumberOfUpgradeSlots(), ModDataComponents.getSourceUpgradeSlots(result));
		assertFalse(source.getTag().hasUUID(BackpackWrapper.CONTENTS_UUID_TAG));
		assertFalse(result.getTag().hasUUID(BackpackWrapper.CONTENTS_UUID_TAG));
	}

	@Test
	void prepareInventoryLayoutUsesNineColumnsForPartialSourceRows() {
		ItemStack source = new ItemStack(GOLD);
		source.getOrCreateTag().putInt("inventorySlots", 80);
		ItemStack result = new ItemStack(DIAMOND);

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, DIAMOND);

		assertEquals(9, ModDataComponents.getSourceColumns(result));
	}

	@Test
	void prepareInventoryLayoutUsesTwelveColumnsAtEightyTwoSlots() {
		ItemStack source = new ItemStack(GOLD);
		source.getOrCreateTag().putInt("inventorySlots", 82);
		ItemStack result = new ItemStack(GOLD);
		result.setTag(source.getTag().copy());

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, GOLD);

		assertEquals(12, ModDataComponents.getSourceColumns(result));
	}

	@Test
	void prepareInventoryLayoutDefersLinkedSourceGeometryToCompletion() {
		ItemStack source = new ItemStack(GOLD);
		source.getOrCreateTag().putInt("inventorySlots", 80);
		LinkedStorageStackData.setEndpoint(source, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		ItemStack result = new ItemStack(DIAMOND);
		result.setTag(source.getTag().copy());

		BackpackUpgradeRecipe.prepareInventoryLayout(source, result, DIAMOND);

		assertNull(ModDataComponents.getSourceColumns(result));
		assertNull(ModDataComponents.getSourceInventorySlots(result));
		assertNull(ModDataComponents.getSourceUpgradeSlots(result));
		assertTrue(ModDataComponents.getOperationId(result) != null);
	}

	@Test
	void prepareInventoryLayoutSkipsMarkersAndWrapperInteractionWhenColumnsDoNotChange() {
		ItemStack source = new ItemStack(DIAMOND);
		ItemStack result = new ItemStack(NETHERITE);

		try (MockedConstruction<BackpackWrapper> wrappers = Mockito.mockConstruction(BackpackWrapper.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			BackpackUpgradeRecipe.prepareInventoryLayout(source, result, NETHERITE);

			assertTrue(wrappers.constructed().isEmpty());
			fitter.verifyNoInteractions();
		}

		assertNull(ModDataComponents.getSourceColumns(result));
		assertNull(ModDataComponents.getSourceInventorySlots(result));
		assertNull(ModDataComponents.getSourceUpgradeSlots(result));
		assertNull(ModDataComponents.getOperationId(result));
		assertFalse(result.getTag().hasUUID(BackpackWrapper.CONTENTS_UUID_TAG));
	}

	@Test
	void completeTierUpgradeMovesConflictingStackAndPreservesCapturedMob() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob capturedMob = capturedMob(17, 2, 2);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMob, 20);

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			IBackpackWrapper upgradedWrapper = new BackpackWrapper(fixture.delivered());
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
			assertEquals(List.of(capturedMob.id()), MobCatcherStorage.getCapturedMobs(upgradedWrapper).stream().map(CapturedMob::id).toList());
			assertEquals(1, countStacks(upgradedWrapper, Items.DIAMOND));
			assertTrue(upgradedWrapper.getInventoryHandler().getStackInSlot(20).isEmpty());
		});
	}

	@Test
	void completeTierUpgradeRepositionsFourCapturedMobsFromGoldToDiamond() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> capturedMobs = List.of(capturedMob(0, 3, 4), capturedMob(3, 3, 4), capturedMob(6, 3, 4), capturedMob(36, 3, 4));
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMobs, List.of());
			assertEquals(48, InventoryLayoutFitter.fit(fixture.upgradedWrapper().getInventoryLayoutParts(9, 12), 108, 12).fittedSlots()
					.get("mob:" + capturedMobs.get(3).id()));

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			IBackpackWrapper upgradedWrapper = new BackpackWrapper(fixture.delivered());
			assertCapturedMobLayout(upgradedWrapper, capturedMobs, List.of(0, 3, 6, 48));
			assertEquals(108, upgradedWrapper.getInventoryHandler().getSlots());
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void completeTierUpgradeRepositionsCapturedMobsWithTwoColumnsTaken() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> capturedMobs = List.of(capturedMob(7, 2, 2), capturedMob(28, 3, 2));
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(capturedMobs, List.of(), 0, 2);
			assertEquals(2, fixture.delivered().getTag().getInt("columnsTaken"));
			assertEquals(2, fixture.upgradedWrapper().getColumnsTaken());
			assertEquals(7, InventoryLayout.getEffectiveColumns(GOLD.getNumberOfSlots(), fixture.upgradedWrapper().getColumnsTaken()));
			assertEquals(63, InventoryLayout.getUsableInventorySlots(GOLD.getNumberOfSlots(), fixture.upgradedWrapper().getColumnsTaken()));
			assertEquals(63, fixture.upgradedWrapper().getInventoryHandler().getSlots());
			assertEquals(7, ModDataComponents.getSourceColumns(fixture.delivered()));

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			IBackpackWrapper upgradedWrapper = new BackpackWrapper(fixture.delivered());
			assertEquals(10, InventoryLayout.getEffectiveColumns(DIAMOND.getNumberOfSlots(), upgradedWrapper.getColumnsTaken()));
			assertEquals(90, InventoryLayout.getUsableInventorySlots(DIAMOND.getNumberOfSlots(), upgradedWrapper.getColumnsTaken()));
			assertCapturedMobLayout(upgradedWrapper, capturedMobs, List.of(10, 40), 10);
			assertEquals(90, upgradedWrapper.getInventoryHandler().getSlots());
			assertEquals(2, fixture.delivered().getTag().getInt("columnsTaken"));
			assertEquals(2, upgradedWrapper.getColumnsTaken());
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void completeTierUpgradeMigratesFromColdCache() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> mobs = fourCapturedMobs();
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(mobs, List.of());

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			assertCapturedMobLayout(new BackpackWrapper(fixture.delivered()), mobs, List.of(0, 3, 6, 48));
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void completeTierUpgradeMigratesOnPreResolvedTargetHandler() throws Throwable {
		runOnServerThread(() -> {
			List<CapturedMob> mobs = fourCapturedMobs();
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(mobs, List.of());
			IBackpackWrapper target = new BackpackWrapper(fixture.delivered());
			assertEquals(108, target.getInventoryHandler().getSlots());

			BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), fixture.player(), fixture.callback(), null);

			assertCapturedMobLayout(target, mobs, List.of(0, 3, 6, 48));
			assertTierUpgradeMarkersRemoved(fixture.delivered(), fixture.callback());
		});
	}

	@Test
	void createDetachedInventoryLayoutViewCopiesCanonicalContentsWithoutMutatingStacks() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob mob = capturedMob(0, 3, 4);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(mob, 20);
			ItemStack deliveredBefore = fixture.delivered().copy();
			UUID uuid = new BackpackWrapper(fixture.delivered()).getContentsUuid().orElseThrow();
			CompoundTag contentsBefore = BackpackStorage.get().getBackpackContents(uuid).orElseThrow().copy();

			BackpackWrapper detached = BackpackWrapper.createDetachedInventoryLayoutView(fixture.delivered(), 81, GOLD.getNumberOfUpgradeSlots());

			assertTrue(detached.getContentsUuid().isEmpty());
			assertEquals(GOLD.getNumberOfUpgradeSlots(), detached.getUpgradeHandler().getSlots());
			assertEquals(List.of(mob), MobCatcherStorage.getCapturedMobs(detached));
			assertTrue(detached.getInventoryLayoutParts(9, 12).stream().anyMatch(part -> part.id().startsWith("mob:")));
			assertTrue(ItemStack.matches(deliveredBefore, fixture.delivered()));
			assertEquals(contentsBefore, BackpackStorage.get().getBackpackContents(uuid).orElseThrow());
		});
	}

	@Test
	void createDetachedInventoryLayoutViewReadsLegacyCanonicalContentsWithoutMutatingStacks() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob mob = capturedMob(0, 3, 4);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(mob, 20);
			UUID uuid = fixture.delivered().getTag().getUUID(BackpackWrapper.CONTENTS_UUID_TAG);
			ItemStack legacy = new ItemStack(DIAMOND);
			legacy.getOrCreateTag().putUUID(BackpackWrapper.CONTENTS_UUID_TAG, uuid);
			ItemStack before = legacy.copy();
			CompoundTag contentsBefore = BackpackStorage.get().getBackpackContents(uuid).orElseThrow().copy();

			BackpackWrapper detached = BackpackWrapper.createDetachedInventoryLayoutView(legacy, 81, GOLD.getNumberOfUpgradeSlots());

			assertEquals(List.of(mob), MobCatcherStorage.getCapturedMobs(detached));
			assertTrue(detached.getInventoryLayoutParts(9, 12).stream().anyMatch(part -> part.id().startsWith("mob:")));
			assertTrue(ItemStack.matches(before, legacy));
			assertEquals(contentsBefore, BackpackStorage.get().getBackpackContents(uuid).orElseThrow());
		});
	}

	@Test
	void createDetachedInventoryLayoutViewUsesRecordedHighestUpgradeSlot() throws Throwable {
		runOnServerThread(() -> {
			CapturedMob mob = capturedMob(0, 3, 4);
			RealWrapperCompletionFixture fixture = createRealWrapperCompletionFixture(mob, 20, GOLD.getNumberOfUpgradeSlots() - 1);

			BackpackWrapper detached = BackpackWrapper.createDetachedInventoryLayoutView(fixture.delivered(), 81, GOLD.getNumberOfUpgradeSlots());

			assertTrue(detached.getUpgradeHandler().getStackInSlot(GOLD.getNumberOfUpgradeSlots() - 1).is(MOB_CATCHER));
			assertTrue(detached.getInventoryLayoutParts(9, 12).stream().anyMatch(part -> part.id().startsWith("mob:")));
		});
	}

	private static List<CapturedMob> fourCapturedMobs() {
		return List.of(capturedMob(0, 3, 4), capturedMob(3, 3, 4), capturedMob(6, 3, 4), capturedMob(36, 3, 4));
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

				IBackpackWrapper upgradedWrapper = new BackpackWrapper(fixture.delivered());
				assertCapturedMobLayout(upgradedWrapper, capturedMobs, testCase.layouts().stream().map(CapturedMobLayout::targetAnchor).toList());
				assertEquals(108, upgradedWrapper.getInventoryHandler().getSlots());
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

			ServerLevel level = Mockito.mock(ServerLevel.class);
			try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
				BackpackUpgradeRecipe.completeTierUpgrade(level, fixture.player(), fixture.callback(), null);
				resolver.verifyNoInteractions();
			}

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
		ItemStack delivered = new ItemStack(DIAMOND);
		ModDataComponents.setSourceColumns(delivered, 9);
		ModDataComponents.setSourceInventorySlots(delivered, 81);
		ModDataComponents.setSourceUpgradeSlots(delivered, GOLD.getNumberOfUpgradeSlots());
		ModDataComponents.setOperationId(delivered, UUID.randomUUID());
		ItemStack callback = delivered.copy();
		Player player = playerWithDeliveredStack(delivered);

		try (MockedStatic<BackpackWrapper> wrappers = Mockito.mockStatic(BackpackWrapper.class)) {
			wrappers.when(() -> BackpackWrapper.createDetachedInventoryLayoutView(delivered, 81, GOLD.getNumberOfUpgradeSlots()))
					.thenThrow(new IllegalStateException("unexpected"));

			assertThrows(IllegalStateException.class, () -> BackpackUpgradeRecipe.completeTierUpgrade(Mockito.mock(ServerLevel.class), player, callback, null));
		}

		assertTierUpgradeMarkersRetained(delivered, callback);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(CapturedMob capturedMob, int occupiedSlot) {
		return createRealWrapperCompletionFixture(capturedMob, occupiedSlot, 0);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(CapturedMob capturedMob, int occupiedSlot, int upgradeSlot) {
		return createRealWrapperCompletionFixture(List.of(capturedMob), List.of(new OccupiedStack(occupiedSlot, new ItemStack(Items.DIAMOND))), upgradeSlot);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks) {
		return createRealWrapperCompletionFixture(capturedMobs, occupiedStacks, 0);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks,
			int upgradeSlot) {
		return createRealWrapperCompletionFixture(capturedMobs, occupiedStacks, upgradeSlot, 0);
	}

	private RealWrapperCompletionFixture createRealWrapperCompletionFixture(List<CapturedMob> capturedMobs, List<OccupiedStack> occupiedStacks, int upgradeSlot,
			int columnsTaken) {
		ItemStack source = new ItemStack(GOLD);
		if (columnsTaken != 0) {
			source.getOrCreateTag().putInt("columnsTaken", columnsTaken);
		}
		BackpackWrapper sourceWrapper = new BackpackWrapper(source);
		sourceWrapper.getInventoryHandler();
		storageUuids.add(sourceWrapper.getContentsUuid().orElseThrow());
		sourceWrapper.setPersistent(true);
		sourceWrapper.getUpgradeHandler().setStackInSlot(upgradeSlot, new ItemStack(MOB_CATCHER));
		capturedMobs.forEach(capturedMob -> MobCatcherStorage.addCapturedMob(sourceWrapper, capturedMob));
		occupiedStacks.forEach(occupiedStack -> sourceWrapper.getInventoryHandler().setSlotStack(occupiedStack.slot(), occupiedStack.stack()));
		persistInventory(sourceWrapper);
		ItemStack delivered = assemble(source, DIAMOND);
		sourceWrapper.replaceBackpackStack(delivered);
		IBackpackWrapper upgradedWrapper = sourceWrapper;
		assertEquals(MOB_CATCHER, upgradedWrapper.getUpgradeHandler().getStackInSlot(upgradeSlot).getItem());
		assertTrue(upgradedWrapper.getInventoryLayoutParts(9, 12).stream().anyMatch(part -> part.id().startsWith("mob:")));
		ItemStack callback = delivered.copy();
		return new RealWrapperCompletionFixture(delivered, callback, playerWithDeliveredStack(delivered), upgradedWrapper);
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
		assertNull(ModDataComponents.getSourceColumns(delivered));
		assertNull(ModDataComponents.getSourceInventorySlots(delivered));
		assertNull(ModDataComponents.getSourceUpgradeSlots(delivered));
		assertNull(ModDataComponents.getOperationId(delivered));
		assertNull(ModDataComponents.getSourceColumns(callback));
		assertNull(ModDataComponents.getSourceInventorySlots(callback));
		assertNull(ModDataComponents.getSourceUpgradeSlots(callback));
		assertNull(ModDataComponents.getOperationId(callback));
	}

	private static void assertTierUpgradeMarkersRetained(ItemStack delivered, ItemStack callback) {
		assertEquals(9, ModDataComponents.getSourceColumns(delivered));
		assertEquals(81, ModDataComponents.getSourceInventorySlots(delivered));
		assertEquals(GOLD.getNumberOfUpgradeSlots(), ModDataComponents.getSourceUpgradeSlots(delivered));
		assertTrue(ModDataComponents.getOperationId(delivered) != null);
		assertEquals(9, ModDataComponents.getSourceColumns(callback));
		assertEquals(81, ModDataComponents.getSourceInventorySlots(callback));
		assertEquals(GOLD.getNumberOfUpgradeSlots(), ModDataComponents.getSourceUpgradeSlots(callback));
		assertTrue(ModDataComponents.getOperationId(callback) != null);
	}

	private record RealWrapperCompletionFixture(ItemStack delivered, ItemStack callback, Player player, IBackpackWrapper upgradedWrapper) {
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
		return new BackpackUpgradeRecipe(new ShapedRecipe(new ResourceLocation("sophisticatedbackpacks", "test_upgrade"), "", CraftingBookCategory.MISC, 1, 1,
				NonNullList.of(Ingredient.EMPTY, Ingredient.of(source)), new ItemStack(result)));
	}

	private static ItemStack assemble(ItemStack source, BackpackItem result) {
		AbstractContainerMenu menu = Mockito.mock(AbstractContainerMenu.class);
		CraftingContainer input = new TransientCraftingContainer(menu, 1, 1);
		input.setItem(0, source);
		return upgradeRecipe((BackpackItem) source.getItem(), result).assemble(input, RegistryAccess.EMPTY);
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
		ItemStack delivered = new ItemStack(DIAMOND);
		LinkedStorageStackData.setEndpoint(delivered, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		ModDataComponents.setOperationId(delivered, UUID.randomUUID());
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
		return new CapturedMob(UUID.randomUUID(), new ResourceLocation("minecraft:pig"), new CompoundTag(), slot, width, height, width * height, false, "Pig",
				10, 10);
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
		wrapper.getBackpackContents().orElseThrow().put(InventoryHandler.INVENTORY_TAG, wrapper.getInventoryHandler().serializeNBT());
	}

	private static void assertInventoryEquals(List<ItemStack> expected, IBackpackWrapper wrapper) {
		for (int slot = 0; slot < expected.size(); slot++) {
			assertTrue(ItemStack.matches(expected.get(slot), wrapper.getInventoryHandler().getStackInSlot(slot)));
		}
	}

	private static BackpackItem registerBackpack(String name, int inventorySlots, int upgradeSlots) {
		return Registry.register(BuiltInRegistries.ITEM, "sophisticatedbackpacks:" + name,
				new BackpackItem(() -> inventorySlots, () -> upgradeSlots, () -> null));
	}

	private static void setItemRegistryFrozen(boolean frozen) {
		boolean foundFrozen = false;
		boolean foundLocked = false;
		for (Class<?> type = BuiltInRegistries.ITEM.getClass(); type != null; type = type.getSuperclass()) {
			for (String name : List.of("frozen", "locked")) {
				try {
					Field field = type.getDeclaredField(name);
					field.setAccessible(true);
					field.setBoolean(BuiltInRegistries.ITEM, frozen);
					foundFrozen |= name.equals("frozen");
					foundLocked |= name.equals("locked");
				} catch (NoSuchFieldException ignored) {
					// The Forge registry wrapper owns these guards on 1.20.1.
				} catch (IllegalAccessException e) {
					throw new IllegalStateException("Unable to register backpack test fixture", e);
				}
			}
		}
		if (!foundFrozen || !foundLocked) {
			throw new IllegalStateException("Unable to locate the item registry write guards");
		}
	}

	private static void runOnServerThread(Runnable action) throws Throwable {
		AtomicReference<Throwable> failure = new AtomicReference<>();
		Thread thread = new Thread(SidedThreadGroups.SERVER, () -> {
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
