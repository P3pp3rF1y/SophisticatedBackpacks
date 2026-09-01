package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.NonNullList;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.network.chat.Component;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.players.PlayerList;
import net.minecraft.util.RandomSource;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.minecraft.world.item.crafting.ShapedRecipePattern;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.neoforged.fml.config.IConfigSpec;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IDiscHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageHostDescriptor;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.DiscHandlerRegistry;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.IJukeboxPlaybackLocationProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxPlaybackLocation;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BackpackLinkedStorageHostWrapperTest {
	private static final RegistryAccess REGISTRY_ACCESS = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);

	@BeforeAll
	static void setup() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		loadDefaultConfig(Config.SERVER_SPEC);
	}

	@Test
	void getInventoryHandlerStoresVirtualHostMutationsOnlyInBinding() {
		TestContentsBinding contents = new TestContentsBinding();
		ItemStack virtualCarrier = new ItemStack(ModItems.BACKPACK.get());

		try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS);
				MockedStatic<BackpackStorage> backpackStorage = Mockito.mockStatic(BackpackStorage.class)) {
			registryHelper.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
			BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, virtualCarrier);

			host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
			host.getSettingsHandler().getTypeCategory(MainSettingsCategory.class).setValue(data -> data.setAnotherPlayerCanOpen(false));
			host.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.ADVANCED_REFILL_UPGRADE.get()));

			assertTrue(contents.contents().inventory().stacks().getFirst().is(Items.DIAMOND));
			assertTrue(contents.contents().settings().getCategoryData(MainSettingsCategory.NAME) != null);
			assertTrue(contents.contents().upgrades().stacks().getFirst().is(ModItems.ADVANCED_REFILL_UPGRADE.get()));
			assertTrue(contents.dirtyCount > 0);
			assertEquals(contents.groupId, host.getContentsUuid().orElseThrow());
			assertFalse(virtualCarrier.has(ModCoreDataComponents.STORAGE_UUID));
			backpackStorage.verifyNoInteractions();
		}
	}

	@Test
	void onLinkedStorageContentsChangedRebindsAllContentHandlers() {
		TestContentsBinding contents = new TestContentsBinding();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, new ItemStack(ModItems.BACKPACK.get()));
		InventoryHandler inventory = host.getInventoryHandler();
		BackpackSettingsHandler settings = host.getSettingsHandler();
		UpgradeHandler upgrades = host.getUpgradeHandler();

		contents.setContents(contents.groupId(), new ContainerContents());
		host.onLinkedStorageContentsChanged();

		assertNotSame(inventory, host.getInventoryHandler());
		assertNotSame(settings, host.getSettingsHandler());
		assertNotSame(upgrades, host.getUpgradeHandler());
	}

	@Test
	void constructorRejectsNonBackpackVirtualCarrier() {
		assertThrows(IllegalArgumentException.class, () -> new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(Items.STICK)));
	}

	@Test
	void linkedStorageBackpackWrapperDelegatesGroupStateAndRetainsPhysicalState() {
		TestContentsBinding contents = new TestContentsBinding();
		ItemStack physicalStack = new ItemStack(ModItems.BACKPACK.get());
		physicalStack.set(DataComponents.CUSTOM_NAME, Component.literal("Secondary Backpack"));
		BackpackWrapper physicalBackpack = new BackpackWrapper(physicalStack);
		ItemStack primaryStack = new ItemStack(ModItems.BACKPACK.get());
		primaryStack.set(DataComponents.CUSTOM_NAME, Component.literal("Primary Backpack"));
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, primaryStack);
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(physicalBackpack, host);
		int physicalInventorySlots = physicalStack.getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 0);
		int[] changes = {0};

		facade.setContentsChangeHandler(() -> changes[0]++);
		facade.setColors(0x112233, 0x445566);
		facade.setOpenTabId(4);
		facade.setSlotNumbers(48, 5);
		facade.setContentsUuid(UUID.randomUUID());
		facade.removeContentsUuid();
		facade.removeContentsUUIDTag();
		facade.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));

		assertSame(host.getInventoryHandler(), facade.getInventoryHandler());
		assertSame(host.getSettingsHandler(), facade.getSettingsHandler());
		assertSame(host.getUpgradeHandler(), facade.getUpgradeHandler());
		assertSame(physicalBackpack.getRenderDataHandler(), facade.getRenderDataHandler());
		assertEquals("Primary Backpack", facade.getDisplayName().getString());
		assertEquals(0x112233, facade.getMainColor());
		assertEquals(0x445566, facade.getAccentColor());
		assertEquals(4, facade.getOpenTabId().orElseThrow());
		assertTrue(host.getOpenTabId().isEmpty());
		assertEquals(48, host.getBackpack().getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 0));
		assertEquals(physicalInventorySlots, physicalStack.getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 0));
		assertEquals(contents.groupId, facade.getContentsUuid().orElseThrow());
		assertTrue(facade.getInventoryHandler().getStackInSlot(0).is(Items.DIAMOND));
		assertFalse(physicalStack.has(ModCoreDataComponents.STORAGE_UUID));
		assertEquals(2, changes[0]);
	}

	@Test
	void onVirtualCarrierChangedReflectsPrimaryCarrierTitleInLinkedFacades() {
		try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
			registryHelper.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
			ItemStack primary = new ItemStack(ModItems.BACKPACK.get());
			primary.set(DataComponents.CUSTOM_NAME, Component.literal("Original Primary"));
			BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), primary);
			LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
			ItemStack renamedPrimary = new ItemStack(ModItems.GOLD_BACKPACK.get());
			renamedPrimary.set(DataComponents.CUSTOM_NAME, Component.literal("Renamed Primary"));

			host.onVirtualCarrierChanged(serialize(renamedPrimary));

			assertEquals("Renamed Primary", facade.getDisplayName().getString());
		}
	}

	@Test
	void onCanonicalContentsChangedKeepsFacadeRefreshCallbacksLocal() {
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get()));
		LinkedStorageBackpackWrapper first = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
		LinkedStorageBackpackWrapper second = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
		int[] firstCallbacks = {0};
		int[] secondCallbacks = {0};

		first.setInventorySlotChangeHandler(() -> firstCallbacks[0]++);
		second.setInventorySlotChangeHandler(() -> secondCallbacks[0]++);
		first.onCanonicalContentsChanged();

		assertEquals(1, firstCallbacks[0]);
		assertEquals(0, secondCallbacks[0]);

		second.onCanonicalContentsChanged();

		assertEquals(1, firstCallbacks[0]);
		assertEquals(1, secondCallbacks[0]);
	}

	@Test
	void onCanonicalContentsChangedSkipsProjectionHandlerForOrdinaryContentsMove() {
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get()));
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
		int[] projectionCallbacks = {0};
		facade.refreshPhysicalProjection();
		RenderData physicalRenderData = facade.getBackpack().get(ModCoreDataComponents.RENDER_DATA);

		host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
		facade.setCanonicalContentsChangedHandler(() -> projectionCallbacks[0]++);
		facade.onCanonicalContentsChanged();

		assertEquals(0, projectionCallbacks[0]);
		assertSame(physicalRenderData, facade.getBackpack().get(ModCoreDataComponents.RENDER_DATA));
	}

	@Test
	void setColumnsTakenProjectsCanonicalColumnsToEveryLinkedFacade() {
		TestContentsBinding contents = new TestContentsBinding();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, new ItemStack(ModItems.BACKPACK.get()));
		LinkedStorageBackpackWrapper primary = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
		LinkedStorageBackpackWrapper secondary = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.GOLD_BACKPACK.get())), host);
		int initialSlots = host.getInventoryHandler().size();
		int[] projectionCallbacks = {0};
		primary.setCanonicalContentsChangedHandler(() -> projectionCallbacks[0]++);

		primary.setColumnsTaken(3, true);
		primary.onContentsUpdated();
		primary.onCanonicalContentsChanged();
		secondary.onCanonicalContentsChanged();

		assertEquals(3, host.getColumnsTaken());
		assertEquals(3, contents.columnsTaken);
		assertEquals(3, primary.getColumnsTaken());
		assertEquals(3, secondary.getColumnsTaken());
		assertEquals(3, primary.getBackpack().getOrDefault(ModDataComponents.COLUMNS_TAKEN, 0));
		assertEquals(3, secondary.getBackpack().getOrDefault(ModDataComponents.COLUMNS_TAKEN, 0));
		assertEquals(initialSlots - (host.getNumberOfSlotRows() * 3), host.getInventoryHandler().size());
		assertEquals(1, projectionCallbacks[0]);
	}

	@Test
	void cloneBackpackStripsLinkedFacadeEndpointIdentity() {
		UUID groupId = UUID.randomUUID();
		ItemStack physicalStack = new ItemStack(ModItems.BACKPACK.get());
		physicalStack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(physicalStack),
				new BackpackLinkedStorageHostWrapper(new TestContentsBinding(groupId), new ItemStack(ModItems.BACKPACK.get())));

		ItemStack clone = facade.cloneBackpack();

		assertEquals(LinkedStorageEndpointStackState.UNLINKED, LinkedStorageStackLifecycle.classifyEndpoint(clone));
		assertNull(clone.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT));
	}

	@Test
	void applyColorsPreservesLinkedBackpackEndpointIdentity() {
		UUID groupId = UUID.randomUUID();
		UUID endpointId = UUID.randomUUID();
		ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, endpointId));
		TestBackpackDyeRecipe recipe = new TestBackpackDyeRecipe();

		recipe.applyColorsForTest(endpoint, List.of(DyeColor.RED), List.of(DyeColor.RED));

		assertEquals(LinkedStorageEndpointStackState.ENDPOINT, LinkedStorageStackLifecycle.classifyEndpoint(endpoint));
		assertEquals(groupId, endpoint.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT).groupId());
		assertEquals(endpointId, endpoint.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT).endpointId());
		assertTrue(endpoint.has(ModCoreDataComponents.MAIN_COLOR));
		assertTrue(endpoint.has(ModCoreDataComponents.ACCENT_COLOR));
	}

	@Test
	void onCanonicalContentsChangedProjectsRenderDataToPhysicalFacade() {
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get()));
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
		host.getRenderDataHandler().setBatteryRenderData(new RenderData.BatteryRenderData(0.5F));
		int[] notifications = {0};
		facade.setCanonicalContentsChangedHandler(() -> notifications[0]++);

		facade.onCanonicalContentsChanged();

		assertEquals(0.5F, facade.getRenderDataHandler().getBatteryRenderData().orElseThrow().chargeRatio());
		assertEquals(1, notifications[0]);
	}

	@Test
	void onCanonicalContentsChangedProjectsItemDisplayRenderDataToPhysicalFacade() {
		try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
			registryHelper.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
			BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get()));
			LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
			int[] projectionCallbacks = {0};
			host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
			host.getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class).selectSlot(0);
			facade.setCanonicalContentsChangedHandler(() -> projectionCallbacks[0]++);
			facade.onCanonicalContentsChanged();

			assertEquals(1, facade.getRenderDataHandler().getDisplayData().displayItems().size());
			assertEquals(host.getRenderDataHandler().getData(), facade.getRenderDataHandler().getData());
			assertEquals(1, projectionCallbacks[0]);
		}
	}

	@Test
	void fromPhysicalStackReadsProjectedEndpointDisplayDataWithoutResolvingContents() {
		try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
			registryHelper.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
			BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get()));
			LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
			host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
			host.getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class).selectSlot(0);
			facade.onCanonicalContentsChanged();

			assertEquals(1, facade.getBackpack().get(ModCoreDataComponents.RENDER_DATA).display().displayItems().size());
		}
	}

	@Test
	void fromPhysicalStackReadsProjectedEndpointTankDataWithoutResolvingContents() {
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get()));
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())), host);
		int[] projectionCallbacks = {0};
		host.getRenderDataHandler().setTankRenderData(TankPosition.LEFT, new RenderData.TankRenderData(net.neoforged.neoforge.fluids.FluidStack.EMPTY, 0.5F));
		facade.setCanonicalContentsChangedHandler(() -> projectionCallbacks[0]++);
		facade.onCanonicalContentsChanged();

		assertTrue(facade.getBackpack().get(ModCoreDataComponents.RENDER_DATA).tanks().containsKey(TankPosition.LEFT));
		assertEquals(1, projectionCallbacks[0]);
	}

	@Test
	void closeUnsubscribesGroupListenerOnce() {
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(new ItemStack(ModItems.BACKPACK.get())),
				new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get())));
		int[] unsubscribes = {0};
		facade.setGroupChangeSubscription(() -> unsubscribes[0]++);

		facade.close();
		facade.close();

		assertEquals(1, unsubscribes[0]);
	}

	@Test
	void hasEndpointRetainsFacadeEndpointIdentityWhenPhysicalStackChanges() {
		LinkedStorageEndpointData originalEndpoint = new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID());
		LinkedStorageEndpointData replacementEndpoint = new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID());
		ItemStack endpointBackpack = new ItemStack(ModItems.BACKPACK.get());
		endpointBackpack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, originalEndpoint);
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(endpointBackpack),
				new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get())));

		endpointBackpack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, replacementEndpoint);

		assertTrue(facade.hasEndpoint(originalEndpoint));
		assertFalse(facade.hasEndpoint(replacementEndpoint));
	}

	@Test
	void getBackpackWrapperClosesNestedLinkedFacadeBeforeReplacingEndpoint() throws ReflectiveOperationException {
		IStorageWrapper parent = Mockito.mock(IStorageWrapper.class);
		InventoryHandler inventory = Mockito.mock(InventoryHandler.class);
		ItemStack replacementStack = linkedEndpoint(UUID.randomUUID());
		LinkedStorageBackpackWrapper oldFacade = new LinkedStorageBackpackWrapper(new BackpackWrapper(linkedEndpoint(UUID.randomUUID())),
				new BackpackLinkedStorageHostWrapper(new TestContentsBinding(), new ItemStack(ModItems.BACKPACK.get())));
		int[] unsubscribes = {0};
		oldFacade.setGroupChangeSubscription(() -> unsubscribes[0]++);
		BackpackContext.ItemSubBackpack context = new BackpackContext.ItemSubBackpack("main", "", 0, false, 0, false);
		Player player = Mockito.mock(Player.class);
		Level level = Mockito.mock(Level.class);
		IBackpackWrapper replacementWrapper = Mockito.mock(IBackpackWrapper.class);

		Mockito.when(parent.getInventoryHandler()).thenReturn(inventory);
		Mockito.when(inventory.getStackInSlot(0)).thenReturn(replacementStack);
		Mockito.when(player.level()).thenReturn(level);
		setContextField(context, "parentWrapper", parent);
		setContextField(context, "backpackWrapper", oldFacade);

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.resolveOrCreate(level, replacementStack)).thenReturn(replacementWrapper);

			assertSame(replacementWrapper, context.getBackpackWrapper(player));
		}

		assertEquals(1, unsubscribes[0]);
	}

	@Test
	void synchronizeRenderProjectionProjectsCarriedEndpointRenderDataWithoutOrdinaryWrapper() {
		UUID groupId = UUID.randomUUID();
		ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(groupId), new ItemStack(ModItems.BACKPACK.get()));
		host.getRenderDataHandler().setBatteryRenderData(new RenderData.BatteryRenderData(0.25F));
		ServerLevel level = Mockito.mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = Mockito.mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = Mockito.mock(LinkedStorageGroupManager.class);

		try (MockedStatic<LinkedStorageGroupsSavedData> groupsSavedData = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groupsSavedData.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);
			Mockito.when(savedData.manager()).thenReturn(manager);
			Mockito.when(manager.isEndpointMember(Mockito.eq(groupId), Mockito.any())).thenReturn(true);
			Mockito.when(manager.resolveVirtualHost(Mockito.any(LinkedStorageEndpointData.class), Mockito.eq(false))).thenReturn(Optional.of(host));
			Mockito.when(manager.getRenderRevision(groupId)).thenReturn(0L, 0L, 1L);

			assertTrue(BackpackLinkedStorageResolver.synchronizeRenderProjection(level, endpoint));
			host.getRenderDataHandler().setBatteryRenderData(new RenderData.BatteryRenderData(0.75F));
			assertFalse(BackpackLinkedStorageResolver.synchronizeRenderProjection(level, endpoint));
			assertEquals(0.25F, endpoint.get(ModCoreDataComponents.RENDER_DATA).battery().orElseThrow().chargeRatio());
			assertTrue(BackpackLinkedStorageResolver.synchronizeRenderProjection(level, endpoint));
		}

		assertEquals(0.75F, endpoint.get(ModCoreDataComponents.RENDER_DATA).battery().orElseThrow().chargeRatio());
		assertEquals(1L, endpoint.get(ModCoreDataComponents.LINKED_STORAGE_RENDER_REVISION));
	}

	@Test
	void getRenderDataHandlerMarksCanonicalBindingRenderDirtyWhenStateChanges() {
		TestContentsBinding contents = new TestContentsBinding();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, new ItemStack(ModItems.BACKPACK.get()));

		host.getRenderDataHandler().setBatteryRenderData(new RenderData.BatteryRenderData(0.5F));

		assertEquals(0, contents.dirtyCount);
		assertEquals(1, contents.renderDirtyCount);
	}

	@Test
	void installSnapshotReplacesRootsAndRejectsOlderRevisions() {
		UUID groupId = UUID.randomUUID();
		ContainerContents staleContents = new ContainerContents();
		staleContents.inventory().stacks().add(new ItemStack(Items.DIRT));
		ContainerContents currentContents = new ContainerContents();
		currentContents.inventory().stacks().add(new ItemStack(Items.DIAMOND));

		ClientLinkedStorageBackpackContents.clear();
		assertTrue(ClientLinkedStorageBackpackContents.getBinding(groupId).isEmpty());
		assertTrue(ClientLinkedStorageBackpackContents.installSnapshot(groupId, 2, currentContents, Component.literal("Main Backpack"),
				new ClientLinkedStorageBackpackContents.StorageSize(36, 4), 0));
		assertFalse(ClientLinkedStorageBackpackContents.installSnapshot(groupId, 1, staleContents, Component.literal("Stale Backpack"),
				new ClientLinkedStorageBackpackContents.StorageSize(27, 3), 0));

		ContainerContents syncedContents = ClientLinkedStorageBackpackContents.getBinding(groupId).orElseThrow().contents();
		assertTrue(syncedContents.inventory().stacks().getFirst().is(Items.DIAMOND));
		assertEquals("Main Backpack", ClientLinkedStorageBackpackContents.getGroupName(groupId).orElseThrow().getString());
		ClientLinkedStorageBackpackContents.clear();
	}

	@Test
	void getBindingRetainsLastSnapshotAfterCacheClear() {
		UUID groupId = UUID.randomUUID();
		ContainerContents contents = new ContainerContents();
		contents.inventory().stacks().add(new ItemStack(Items.DIAMOND));

		ClientLinkedStorageBackpackContents.clear();
		ClientLinkedStorageBackpackContents.installSnapshot(groupId, 1, contents, Component.literal("Main Backpack"),
				new ClientLinkedStorageBackpackContents.StorageSize(36, 4), 2);
		ILinkedStorageContentsBinding binding = ClientLinkedStorageBackpackContents.getBinding(groupId).orElseThrow();
		ClientLinkedStorageBackpackContents.clear();

		assertTrue(binding.contents().inventory().stacks().getFirst().is(Items.DIAMOND));
		assertEquals(2, binding.getColumnsTaken());
	}

	@Test
	void getGroupNameSharesClientGroupNameAcrossEndpointTooltips() {
		UUID groupId = UUID.randomUUID();

		ClientLinkedStorageBackpackContents.clear();
		ClientLinkedStorageBackpackContents.installSnapshot(groupId, 1, new ContainerContents(), Component.literal("Main Backpack"),
				new ClientLinkedStorageBackpackContents.StorageSize(36, 4), 0);

		assertEquals("Main Backpack", ClientLinkedStorageBackpackContents.getGroupName(groupId).orElseThrow().getString());
		ClientLinkedStorageBackpackContents.clear();
	}

	@Test
	void copyCanonicalContentsMigratesOrdinaryRoot() {
		ItemStack physicalStack = new ItemStack(ModItems.BACKPACK.get());
		UUID ordinaryContentsId = UUID.randomUUID();
		ContainerContents ordinaryContents = new ContainerContents();
		ordinaryContents.inventory().stacks().add(new ItemStack(Items.DIAMOND));
		physicalStack.set(ModCoreDataComponents.STORAGE_UUID, ordinaryContentsId);
		BackpackStorage storage = Mockito.mock(BackpackStorage.class);
		ServerLevel level = Mockito.mock(ServerLevel.class);

		try (MockedStatic<BackpackStorage> backpackStorage = Mockito.mockStatic(BackpackStorage.class)) {
			backpackStorage.when(BackpackStorage::get).thenReturn(storage);
			Mockito.when(storage.getOrCreateBackpackContents(ordinaryContentsId)).thenReturn(ordinaryContents);
			BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();

			ContainerContents canonicalRoot = adapter.copyCanonicalContents(level, physicalStack);

			assertTrue(canonicalRoot.inventory().stacks().getFirst().is(Items.DIAMOND));
			assertTrue(physicalStack.has(ModCoreDataComponents.STORAGE_UUID));
			Mockito.verify(storage, Mockito.never()).removeBackpackContents(ordinaryContentsId);
		}
	}

	@Test
	void backpackContainerExposesItsContextForEndpointLinkClosure() {
		assertTrue(IContextAwareContainer.class.isAssignableFrom(BackpackContainer.class));
	}

	@Test
	void onEndpointLinkedClosesOnlyMenusShowingTheLinkedStack() {
		ServerLevel level = Mockito.mock(ServerLevel.class);
		MinecraftServer server = Mockito.mock(MinecraftServer.class);
		PlayerList playerList = Mockito.mock(PlayerList.class);
		ServerPlayer linkedPlayer = Mockito.mock(ServerPlayer.class);
		ServerPlayer unrelatedPlayer = Mockito.mock(ServerPlayer.class);
		ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
		ItemStack unrelatedEndpoint = new ItemStack(ModItems.BACKPACK.get());
		IContextAwareContainer linkedContainer = createContextAwareContainer(linkedPlayer, endpoint);
		IContextAwareContainer unrelatedContainer = createContextAwareContainer(unrelatedPlayer, unrelatedEndpoint);

		Mockito.when(level.getServer()).thenReturn(server);
		Mockito.when(server.getPlayerList()).thenReturn(playerList);
		Mockito.when(playerList.getPlayers()).thenReturn(List.of(linkedPlayer, unrelatedPlayer));
		Mockito.when(linkedPlayer.level()).thenReturn(level);
		Mockito.when(unrelatedPlayer.level()).thenReturn(level);
		linkedPlayer.containerMenu = (AbstractContainerMenu) linkedContainer;
		unrelatedPlayer.containerMenu = (AbstractContainerMenu) unrelatedContainer;

		new BackpackLinkedStorageEndpointAdapter().onEndpointLinked(level, endpoint);

		Mockito.verify(linkedPlayer).closeContainer();
		Mockito.verify(unrelatedPlayer, Mockito.never()).closeContainer();
	}

	private static IContextAwareContainer createContextAwareContainer(ServerPlayer player, ItemStack backpack) {
		AbstractContainerMenu menu = Mockito.mock(AbstractContainerMenu.class, Mockito.withSettings().extraInterfaces(IContextAwareContainer.class));
		IContextAwareContainer contextAwareContainer = (IContextAwareContainer) menu;
		BackpackContext context = Mockito.mock(BackpackContext.class);
		IBackpackWrapper wrapper = Mockito.mock(IBackpackWrapper.class);
		Mockito.when(contextAwareContainer.getBackpackContext()).thenReturn(context);
		Mockito.when(context.getBackpackWrapper(player)).thenReturn(wrapper);
		Mockito.when(wrapper.getBackpack()).thenReturn(backpack);
		return contextAwareContainer;
	}

	@Test
	void getCompatibilityRejectsSecondaryCandidatesWithItemsWithoutMutatingOrdinaryStorage() {
		ServerLevel level = Mockito.mock(ServerLevel.class);
		BackpackStorage storage = Mockito.mock(BackpackStorage.class);
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		LinkedStorageHostDescriptor descriptor = new LinkedStorageHostDescriptor(adapter.factoryId(), new CompoundTag());
		ContainerContents inventoryRoot = new ContainerContents();
		inventoryRoot.inventory().stacks().add(new ItemStack(Items.DIAMOND));
		ContainerContents upgradeRoot = new ContainerContents();
		upgradeRoot.upgrades().stacks().add(new ItemStack(ModItems.ADVANCED_REFILL_UPGRADE.get()));
		ContainerContents emptyRoot = new ContainerContents();
		ContainerContents settingsRoot = new ContainerContents();
		ContainerContents partitionerRoot = new ContainerContents(new ContainerContents.InventoryData(),
				new ContainerContents.PartitionerData(new int[]{0}, List.of("partition")), new ContainerContents.UpgradeData(),
				new ContainerContents.SettingsData());

		try (MockedStatic<BackpackStorage> backpackStorage = Mockito.mockStatic(BackpackStorage.class, Mockito.CALLS_REAL_METHODS)) {
			backpackStorage.when(BackpackStorage::get).thenReturn(storage);
			for (ContainerContents contents : List.of(inventoryRoot, upgradeRoot)) {
				UUID contentsId = UUID.randomUUID();
				ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
				endpoint.set(ModCoreDataComponents.STORAGE_UUID, contentsId);
				Mockito.when(storage.getOrCreateBackpackContents(contentsId)).thenReturn(contents);

				assertFalse(adapter.isCompatible(level, endpoint, descriptor));
				assertEquals(ILinkedStorageEndpointAdapter.Compatibility.HAS_CONTENTS, adapter.getCompatibility(level, endpoint, descriptor));
			}

			ItemStack emptyEndpoint = new ItemStack(ModItems.BACKPACK.get());
			assertTrue(adapter.isCompatible(level, emptyEndpoint, descriptor));
			UUID emptyContentsId = UUID.randomUUID();
			emptyEndpoint.set(ModCoreDataComponents.STORAGE_UUID, emptyContentsId);
			Mockito.when(storage.getOrCreateBackpackContents(emptyContentsId)).thenReturn(emptyRoot);

			assertTrue(adapter.isCompatible(level, emptyEndpoint, descriptor));
			for (ContainerContents contents : List.of(settingsRoot, partitionerRoot)) {
				UUID contentsId = UUID.randomUUID();
				ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
				endpoint.set(ModCoreDataComponents.STORAGE_UUID, contentsId);
				Mockito.when(storage.getOrCreateBackpackContents(contentsId)).thenReturn(contents);

				assertTrue(adapter.isCompatible(level, endpoint, descriptor));
			}

			Mockito.verify(storage, Mockito.never()).removeBackpackContents(Mockito.any());
		}
	}

	@Test
	void getCompatibilityAllowsAllocatedButEmptyInventoryAndUpgrades() {
		ServerLevel level = Mockito.mock(ServerLevel.class);
		BackpackStorage storage = Mockito.mock(BackpackStorage.class);
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		LinkedStorageHostDescriptor descriptor = new LinkedStorageHostDescriptor(adapter.factoryId(), new CompoundTag());
		ContainerContents allocatedEmptyContents = new ContainerContents(new ContainerContents.InventoryData(NonNullList.withSize(1, ItemStack.EMPTY)),
				new ContainerContents.PartitionerData(), new ContainerContents.UpgradeData(NonNullList.withSize(1, ItemStack.EMPTY)),
				new ContainerContents.SettingsData());
		UUID contentsId = UUID.randomUUID();
		ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
		endpoint.set(ModCoreDataComponents.STORAGE_UUID, contentsId);

		try (MockedStatic<BackpackStorage> backpackStorage = Mockito.mockStatic(BackpackStorage.class, Mockito.CALLS_REAL_METHODS)) {
			backpackStorage.when(BackpackStorage::get).thenReturn(storage);
			Mockito.when(storage.getOrCreateBackpackContents(contentsId)).thenReturn(allocatedEmptyContents);

			assertTrue(adapter.isCompatible(level, endpoint, descriptor));
			assertEquals(ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE, adapter.getCompatibility(level, endpoint, descriptor));
		}
	}

	@Test
	void bindEndpointUsesPrimaryGroupStorageSizeForDifferentTierEndpoint() {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		primary.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, ModItems.DIAMOND_BACKPACK.get().getNumberOfSlots());
		primary.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, ModItems.DIAMOND_BACKPACK.get().getNumberOfUpgradeSlots());
		ItemStack secondary = new ItemStack(ModItems.GOLD_BACKPACK.get());
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		ServerLevel level = Mockito.mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = Mockito.mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = Mockito.mock(LinkedStorageGroupManager.class);
		LinkedStorageHostDescriptor descriptor = new LinkedStorageHostDescriptor(adapter.factoryId(), serialize(primary));

		Mockito.when(level.registryAccess()).thenReturn(REGISTRY_ACCESS);
		Mockito.when(savedData.manager()).thenReturn(manager);
		Mockito.when(manager.getHostDescriptor(groupId)).thenReturn(Optional.of(descriptor));
		try (MockedStatic<LinkedStorageGroupsSavedData> groupsSavedData = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groupsSavedData.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);
			adapter.bindEndpoint(level, secondary, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		}

		assertNotSame(primary.getItem(), secondary.getItem());
		assertEquals(primary.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS), secondary.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS));
		assertEquals(primary.get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS), secondary.get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS));
		ItemStack clientVirtualCarrier = secondary.copy();
		clientVirtualCarrier.remove(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		BackpackLinkedStorageHostWrapper clientHost = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(groupId), clientVirtualCarrier);
		assertEquals(primary.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS), clientHost.getInventoryHandler().size());
	}

	@Test
	void bindEndpointTransfersPrimaryColumnsTakenToCanonicalContentsBinding() {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = new ItemStack(ModItems.BACKPACK.get());
		primary.set(ModDataComponents.COLUMNS_TAKEN, 3);
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		ServerLevel level = Mockito.mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = Mockito.mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = Mockito.mock(LinkedStorageGroupManager.class);
		TestContentsBinding canonicalContents = new TestContentsBinding(groupId);
		LinkedStorageHostDescriptor descriptor = new LinkedStorageHostDescriptor(adapter.factoryId(), serialize(new ItemStack(ModItems.BACKPACK.get())));

		Mockito.when(level.registryAccess()).thenReturn(REGISTRY_ACCESS);
		Mockito.when(savedData.manager()).thenReturn(manager);
		Mockito.when(manager.getHostDescriptor(groupId)).thenReturn(Optional.of(descriptor));
		Mockito.when(manager.isPrimaryEndpoint(Mockito.eq(groupId), Mockito.any())).thenReturn(true);
		Mockito.when(manager.resolveContents(groupId)).thenReturn(Optional.of(canonicalContents));
		try (MockedStatic<LinkedStorageGroupsSavedData> groupsSavedData = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groupsSavedData.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);
			adapter.bindEndpoint(level, primary, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		}

		assertEquals(3, canonicalContents.getColumnsTaken());
	}

	@Test
	void matchesAllowsTierUpgradeRecipesOnlyForPrimaryEndpoints() {
		BackpackUpgradeRecipe recipe = upgradeRecipe(ModItems.BACKPACK.get(), ModItems.GOLD_BACKPACK.get());
		ItemStack secondary = new ItemStack(ModItems.BACKPACK.get());
		secondary.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		secondary.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, false);

		assertFalse(recipe.matches(CraftingInput.of(1, 1, List.of(secondary)), null));

		secondary.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
		assertTrue(recipe.matches(CraftingInput.of(1, 1, List.of(secondary)), null));
	}

	@Test
	void getLinkedStorageEndpointRoleUsesEndpointLocalPrimaryMarker() {
		ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));

		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
		assertEquals(LinkedStorageEndpointRole.PRIMARY, BackpackItem.getLinkedStorageEndpointRole(endpoint).orElseThrow());
		assertEquals(LinkedStorageEndpointRole.PRIMARY, new BackpackWrapper(endpoint).getLinkedStorageEndpointRole().orElseThrow());

		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, false);
		assertEquals(LinkedStorageEndpointRole.SECONDARY, BackpackItem.getLinkedStorageEndpointRole(endpoint).orElseThrow());
		assertEquals(LinkedStorageEndpointRole.SECONDARY, new BackpackWrapper(endpoint).getLinkedStorageEndpointRole().orElseThrow());
	}

	@Test
	void shouldRenderUpgradeActivityAllowsOrdinaryAndPrimaryBackpacksOnly() {
		ItemStack ordinary = new ItemStack(ModItems.BACKPACK.get());
		ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));

		assertTrue(BackpackItem.shouldRenderUpgradeActivity(ordinary));

		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
		assertTrue(BackpackItem.shouldRenderUpgradeActivity(endpoint));

		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, false);
		assertFalse(BackpackItem.shouldRenderUpgradeActivity(endpoint));
	}

	@Test
	void completePrimaryTierUpgradeUpdatesCanonicalCarrierProfile() {
		UUID groupId = UUID.randomUUID();
		UUID endpointId = UUID.randomUUID();
		ItemStack source = new ItemStack(ModItems.BACKPACK.get());
		source.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, endpointId));
		source.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
		ItemStack result = new ItemStack(ModItems.GOLD_BACKPACK.get());
		result.applyComponents(source.getComponents());
		ServerLevel level = Mockito.mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = Mockito.mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = Mockito.mock(LinkedStorageGroupManager.class);
		LinkedStorageHostDescriptor descriptor = new LinkedStorageHostDescriptor(BackpackLinkedStorageHostWrapper.FACTORY_ID, serialize(source));

		Mockito.when(level.registryAccess()).thenReturn(REGISTRY_ACCESS);
		Mockito.when(savedData.manager()).thenReturn(manager);
		Mockito.when(manager.isPrimaryEndpoint(groupId, endpointId)).thenReturn(true);
		Mockito.when(manager.getHostDescriptor(groupId)).thenReturn(Optional.of(descriptor));
		try (MockedStatic<LinkedStorageGroupsSavedData> groupsSavedData = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groupsSavedData.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);

			assertTrue(BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(level, result, new SimpleContainer(source)));
		}

		assertEquals(ModItems.GOLD_BACKPACK.get().getNumberOfSlots(), result.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS));
		assertEquals(ModItems.GOLD_BACKPACK.get().getNumberOfUpgradeSlots(), result.get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS));
		ArgumentCaptor<LinkedStorageHostDescriptor> descriptorCaptor = ArgumentCaptor.forClass(LinkedStorageHostDescriptor.class);
		Mockito.verify(manager).updatePrimaryHostDescriptor(Mockito.eq(groupId), Mockito.eq(endpointId), descriptorCaptor.capture());
		ItemStack upgradedCarrier = ItemStack.CODEC.parse(NbtOps.INSTANCE, descriptorCaptor.getValue().virtualCarrier()).getOrThrow();
		assertTrue(upgradedCarrier.is(ModItems.GOLD_BACKPACK.get()));
		assertEquals(ModItems.GOLD_BACKPACK.get().getNumberOfSlots(), upgradedCarrier.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS));
	}

	@Test
	void resolveUsesServerCanonicalHostForEndpoint() {
		UUID groupId = UUID.randomUUID();
		ItemStack physicalStack = new ItemStack(ModItems.BACKPACK.get());
		physicalStack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(groupId), new ItemStack(ModItems.BACKPACK.get()));
		ServerLevel level = Mockito.mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = Mockito.mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = Mockito.mock(LinkedStorageGroupManager.class);

		try (MockedStatic<LinkedStorageGroupsSavedData> groupsSavedData = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groupsSavedData.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);
			Mockito.when(savedData.manager()).thenReturn(manager);
			Mockito.when(manager.isEndpointMember(Mockito.eq(groupId), Mockito.any())).thenReturn(true);
			Mockito.when(manager.isPrimaryEndpoint(Mockito.eq(groupId), Mockito.any())).thenReturn(false);
			Mockito.when(manager.resolveVirtualHost(groupId)).thenReturn(Optional.of(host));
			Mockito.when(manager.getHostDescriptor(groupId)).thenReturn(Optional
					.of(new LinkedStorageHostDescriptor(BackpackLinkedStorageHostWrapper.FACTORY_ID, serialize(new ItemStack(ModItems.BACKPACK.get())))));
			Mockito.when(level.registryAccess()).thenReturn(REGISTRY_ACCESS);
			Mockito.when(manager.subscribeToGroupChanges(Mockito.eq(groupId), Mockito.any())).thenReturn(() -> {
			});

			IBackpackWrapper resolved = BackpackLinkedStorageResolver.resolve(level, physicalStack).orElseThrow();

			assertSame(host.getInventoryHandler(), resolved.getInventoryHandler());
			assertEquals(groupId, resolved.getContentsUuid().orElseThrow());
			Mockito.verify(manager).subscribeToGroupChanges(Mockito.eq(groupId), Mockito.any());
		}
	}

	@Test
	void resolveCanonicalHostResolvesSecondaryEndpointForCapabilitiesButNotGlobalUpgradeProcessing() {
		UUID groupId = UUID.randomUUID();
		UUID endpointId = UUID.randomUUID();
		ItemStack secondaryStack = new ItemStack(ModItems.BACKPACK.get());
		secondaryStack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, endpointId));
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContentsBinding(groupId), new ItemStack(ModItems.BACKPACK.get()));
		ServerLevel level = Mockito.mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = Mockito.mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = Mockito.mock(LinkedStorageGroupManager.class);

		try (MockedStatic<LinkedStorageGroupsSavedData> groupsSavedData = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groupsSavedData.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);
			Mockito.when(savedData.manager()).thenReturn(manager);
			Mockito.when(manager.isEndpointMember(groupId, endpointId)).thenReturn(true);
			Mockito.when(manager.isPrimaryEndpoint(groupId, endpointId)).thenReturn(false);
			Mockito.when(manager.resolveVirtualHost(groupId)).thenReturn(Optional.of(host));

			assertSame(host, BackpackLinkedStorageResolver.resolveCanonicalHost(level, secondaryStack).orElseThrow());
			assertTrue(BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, secondaryStack).isEmpty());
			assertSame(IBackpackWrapper.Noop.INSTANCE, BackpackLinkedStorageResolver.resolveForGlobalUpgradeProcessing(level, secondaryStack));
		}
	}

	@Test
	void resolveUsesCachedCanonicalStorageSizeOnClient() {
		UUID groupId = UUID.randomUUID();
		ItemStack secondary = new ItemStack(ModItems.GOLD_BACKPACK.get());
		secondary.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		ClientLinkedStorageBackpackContents.clear();
		ClientLinkedStorageBackpackContents.installSnapshot(groupId, 1, new ContainerContents(), Component.empty(),
				new ClientLinkedStorageBackpackContents.StorageSize(ModItems.BACKPACK.get().getNumberOfSlots(),
						ModItems.BACKPACK.get().getNumberOfUpgradeSlots()),
				2);

		IBackpackWrapper resolved = BackpackLinkedStorageResolver.resolve(Mockito.mock(Level.class), secondary).orElseThrow();

		assertEquals(ModItems.BACKPACK.get().getNumberOfSlots() - 2 * resolved.getNumberOfSlotRows(), resolved.getInventoryHandler().size());
		assertEquals(ModItems.BACKPACK.get().getNumberOfUpgradeSlots(), resolved.getUpgradeHandler().size());
		ClientLinkedStorageBackpackContents.clear();
	}

	@Test
	void inventoryTickDispatchesOnlyPrimaryLinkedEndpoint() {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = linkedEndpoint(groupId);
		ItemStack secondary = linkedEndpoint(groupId);
		ServerLevel level = Mockito.mock(ServerLevel.class);
		Player player = Mockito.mock(Player.class);
		BlockPos playerPos = new BlockPos(4, 70, 9);
		IBackpackWrapper canonicalHost = Mockito.mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = Mockito.mock(UpgradeHandler.class);
		ITickableUpgrade tickableUpgrade = Mockito.mock(ITickableUpgrade.class);

		Mockito.when(player.isSpectator()).thenReturn(false);
		Mockito.when(player.isDeadOrDying()).thenReturn(false);
		Mockito.when(player.level()).thenReturn(level);
		Mockito.when(player.blockPosition()).thenReturn(playerPos);
		Mockito.when(canonicalHost.getUpgradeHandler()).thenReturn(upgrades);
		Mockito.when(upgrades.getWrappersThatImplement(ITickableUpgrade.class)).thenReturn(List.of(tickableUpgrade));

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.synchronizeRenderProjection(level, primary)).thenReturn(false);
			resolver.when(() -> BackpackLinkedStorageResolver.synchronizeRenderProjection(level, secondary)).thenReturn(false);
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, primary)).thenReturn(Optional.of(canonicalHost));
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, secondary)).thenReturn(Optional.empty());

			ModItems.BACKPACK.get().inventoryTick(primary, level, player, null);
			ModItems.BACKPACK.get().inventoryTick(secondary, level, player, EquipmentSlot.CHEST);
		}

		Mockito.verify(tickableUpgrade).tick(player, level, playerPos);
		Mockito.verifyNoMoreInteractions(tickableUpgrade);
	}

	@Test
	void serverTickDispatchesOnlyPrimaryLinkedEndpoint() throws ReflectiveOperationException {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = linkedEndpoint(groupId);
		ItemStack secondary = linkedEndpoint(groupId);
		ServerLevel level = Mockito.mock(ServerLevel.class);
		BlockPos primaryPos = new BlockPos(1, 64, 1);
		BlockPos secondaryPos = new BlockPos(2, 64, 1);
		IBackpackWrapper canonicalHost = Mockito.mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = Mockito.mock(UpgradeHandler.class);
		ITickableUpgrade tickableUpgrade = Mockito.mock(ITickableUpgrade.class);
		IBackpackWrapper primaryPhysical = Mockito.mock(IBackpackWrapper.class);
		IBackpackWrapper secondaryPhysical = Mockito.mock(IBackpackWrapper.class);

		Mockito.when(canonicalHost.getUpgradeHandler()).thenReturn(upgrades);
		Mockito.when(upgrades.getWrappersThatImplement(ITickableUpgrade.class)).thenReturn(List.of(tickableUpgrade));
		Mockito.when(primaryPhysical.getBackpack()).thenReturn(primary);
		Mockito.when(secondaryPhysical.getBackpack()).thenReturn(secondary);
		BackpackBlockEntity primaryBlock = new BackpackBlockEntity(primaryPos, ModBlocks.BACKPACK.get().defaultBlockState());
		BackpackBlockEntity secondaryBlock = new BackpackBlockEntity(secondaryPos, ModBlocks.BACKPACK.get().defaultBlockState());
		setBackpackWrapper(primaryBlock, primaryPhysical);
		setBackpackWrapper(secondaryBlock, secondaryPhysical);

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, primary)).thenReturn(Optional.of(canonicalHost));
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, secondary)).thenReturn(Optional.empty());

			BackpackBlockEntity.serverTick(level, primaryPos, primaryBlock);
			BackpackBlockEntity.serverTick(level, secondaryPos, secondaryBlock);
		}

		Mockito.verify(tickableUpgrade).tick(null, level, primaryPos);
		Mockito.verifyNoMoreInteractions(tickableUpgrade);
	}

	@Test
	void inventoryTickSkipsNonPlayerAndInactiveLinkedEndpoints() {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = linkedEndpoint(groupId);
		ServerLevel level = Mockito.mock(ServerLevel.class);
		Entity nonPlayer = Mockito.mock(Entity.class);
		Player player = Mockito.mock(Player.class);
		IBackpackWrapper canonicalHost = Mockito.mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = Mockito.mock(UpgradeHandler.class);
		ITickableUpgrade tickableUpgrade = Mockito.mock(ITickableUpgrade.class);
		boolean onlyWornBefore = Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get();

		Mockito.when(player.isSpectator()).thenReturn(false);
		Mockito.when(player.isDeadOrDying()).thenReturn(false);
		Mockito.when(player.level()).thenReturn(level);
		Mockito.when(player.blockPosition()).thenReturn(BlockPos.ZERO);
		Mockito.when(canonicalHost.getUpgradeHandler()).thenReturn(upgrades);
		Mockito.when(upgrades.getWrappersThatImplement(ITickableUpgrade.class)).thenReturn(List.of(tickableUpgrade));

		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.synchronizeRenderProjection(level, primary)).thenReturn(false);
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, primary)).thenReturn(Optional.of(canonicalHost));

			ModItems.BACKPACK.get().inventoryTick(primary, level, nonPlayer, null);
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(true);
			ModItems.BACKPACK.get().inventoryTick(primary, level, player, null);
		} finally {
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(onlyWornBefore);
		}

		Mockito.verifyNoInteractions(tickableUpgrade);
	}

	@Test
	void playUsesProvidedPrimaryAnchorInsteadOfInitiatingEndpoint() {
		ServerLevel initiatingLevel = Mockito.mock(ServerLevel.class);
		ServerLevel primaryLevel = Mockito.mock(ServerLevel.class);
		BlockPos initiatingPos = new BlockPos(1, 64, 1);
		BlockPos primaryPos = new BlockPos(9, 70, -3);
		UUID storageUuid = UUID.randomUUID();
		IStorageWrapper storageWrapper = Mockito.mock(IStorageWrapper.class, Mockito.withSettings().extraInterfaces(IJukeboxPlaybackLocationProvider.class));
		IJukeboxPlaybackLocationProvider playbackLocationProvider = (IJukeboxPlaybackLocationProvider) storageWrapper;
		net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler renderDataHandler = Mockito
				.mock(net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler.class);

		Mockito.when(storageWrapper.getContentsUuid()).thenReturn(Optional.of(storageUuid));
		Mockito.when(storageWrapper.getRenderDataHandler()).thenReturn(renderDataHandler);
		Mockito.when(playbackLocationProvider.getJukeboxPlaybackLocation(initiatingLevel))
				.thenReturn(Optional.of(JukeboxPlaybackLocation.forBlock(primaryLevel, primaryPos)));

		TestJukeboxUpgradeWrapper wrapper = new TestJukeboxUpgradeWrapper(storageWrapper, new ItemStack(ModItems.JUKEBOX_UPGRADE.get()));
		AtomicReference<BlockPos> playedAt = new AtomicReference<>();
		IDiscHandler<Object> handler = new IDiscHandler<>() {
			@Override
			public Optional<Object> getSongInfo(ItemStack itemStack, Level level) {
				return Optional.empty();
			}

			@Override
			public void playDisc(ServerLevel level, BlockPos pos, UUID uuid, ItemStack disc, Runnable onFinished) {
				assertEquals(primaryLevel, level);
				assertEquals(storageUuid, uuid);
				playedAt.set(pos);
			}

			@Override
			public void playDisc(ServerLevel level, Vec3 pos, UUID uuid, ItemStack disc, int entityId, Runnable onFinished) {
				throw new AssertionError("Expected block playback");
			}

			@Override
			public Optional<Integer> getMusicLengthInTicks(ItemStack itemStack, Level level) {
				return Optional.empty();
			}

			@Override
			public boolean supports(ItemStack itemStack) {
				return itemStack.is(Items.STICK);
			}

			@Override
			public Optional<ItemStack> getRandomDisc(RandomSource randomSource) {
				return Optional.empty();
			}

			@Override
			public int getMusicDiscSize() {
				return 0;
			}
		};

		DiscHandlerRegistry.registerHandler(handler);
		try {
			try (Transaction tx = Transaction.openRoot()) {
				wrapper.getDiscInventory().insert(0, ItemResource.of(new ItemStack(Items.STICK)), 1, tx);
				tx.commit();
			}
			wrapper.play(initiatingLevel, initiatingPos);
		} finally {
			DiscHandlerRegistry.getHandlers().remove(handler);
		}

		assertEquals(primaryPos, playedAt.get());
	}

	@Test
	void isPrimaryEndpointRejectsSecondaryLinkedBackpack() {
		UUID groupId = UUID.randomUUID();
		UUID endpointId = UUID.randomUUID();
		ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
		endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, endpointId));
		ServerLevel level = Mockito.mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = Mockito.mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = Mockito.mock(LinkedStorageGroupManager.class);

		Mockito.when(savedData.manager()).thenReturn(manager);
		Mockito.when(manager.isPrimaryEndpoint(groupId, endpointId)).thenReturn(false);
		try (MockedStatic<LinkedStorageGroupsSavedData> groupsSavedData = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groupsSavedData.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);

			assertFalse(LinkedStorageJukeboxPlaybackAnchors.isPrimaryEndpoint(level, endpoint));
		}
	}

	private static ItemStack linkedEndpoint(UUID groupId) {
		ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
		stack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		return stack;
	}

	private static void setBackpackWrapper(BackpackBlockEntity blockEntity, IBackpackWrapper backpackWrapper) throws ReflectiveOperationException {
		Field field = BackpackBlockEntity.class.getDeclaredField("backpackWrapper");
		field.setAccessible(true);
		field.set(blockEntity, backpackWrapper);
	}

	private static void setContextField(Object context, String fieldName, Object value) throws ReflectiveOperationException {
		Field field = context.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(context, value);
	}

	private static class TestJukeboxUpgradeWrapper extends JukeboxUpgradeWrapper {
		private TestJukeboxUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade) {
			super(storageWrapper, upgrade, ignored -> {
			});
		}
	}

	private static void loadDefaultConfig(ModConfigSpec configSpec) throws ReflectiveOperationException {
		if (configSpec.isLoaded()) {
			return;
		}

		CommentedConfig config = CommentedConfig.inMemory();
		configSpec.correct(config);
		Class<?> loadedConfigClass = Class.forName("net.neoforged.fml.config.LoadedConfig");
		Constructor<?> constructor = loadedConfigClass.getDeclaredConstructor(CommentedConfig.class, Path.class, ModConfig.class);
		constructor.setAccessible(true);
		configSpec.acceptConfig((IConfigSpec.ILoadedConfig) constructor.newInstance(config, null, null));
	}

	private static BackpackUpgradeRecipe upgradeRecipe(BackpackItem source, BackpackItem result) {
		return new BackpackUpgradeRecipe(new ShapedRecipe("", CraftingBookCategory.MISC,
				new ShapedRecipePattern(1, 1, NonNullList.of(Optional.empty(), Optional.of(Ingredient.of(source))), Optional.empty()), new ItemStack(result)));
	}

	private static CompoundTag serialize(ItemStack stack) {
		return (CompoundTag) ItemStack.CODEC.encodeStart(NbtOps.INSTANCE, stack).getOrThrow();
	}

	private static class TestBackpackDyeRecipe extends BackpackDyeRecipe {
		private TestBackpackDyeRecipe() {
			super(CraftingBookCategory.MISC);
		}

		private void applyColorsForTest(ItemStack stack, List<DyeColor> mainDyes, List<DyeColor> trimDyes) {
			applyColors(stack, mainDyes, trimDyes);
		}
	}

	private static class TestContentsBinding implements ILinkedStorageContentsBinding {
		private final UUID groupId;
		private ContainerContents contents = new ContainerContents();
		private int dirtyCount;
		private int renderDirtyCount;
		private int columnsTaken;

		private TestContentsBinding() {
			this(UUID.randomUUID());
		}

		private TestContentsBinding(UUID groupId) {
			this.groupId = groupId;
		}

		@Override
		public UUID groupId() {
			return groupId;
		}

		@Override
		public ContainerContents getContents(UUID storageId) {
			return contents;
		}

		@Override
		public void setContents(UUID storageId, ContainerContents contents) {
			this.contents = contents;
		}

		@Override
		public void markChanged() {
			dirtyCount++;
		}

		@Override
		public void markRenderDirty() {
			renderDirtyCount++;
		}

		@Override
		public int getColumnsTaken() {
			return columnsTaken;
		}

		@Override
		public void setColumnsTaken(int columnsTaken) {
			this.columnsTaken = columnsTaken;
		}
	}
}
