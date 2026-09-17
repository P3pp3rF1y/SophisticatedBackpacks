package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.NonNullList;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.players.PlayerList;
import net.minecraft.util.RandomSource;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.ListenerList;
import net.minecraftforge.eventbus.LockHelper;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventListenerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.api.IDiscHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ClientLinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageHostDescriptor;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.DiscHandlerRegistry;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.IJukeboxPlaybackLocationProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxPlaybackLocation;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeWrapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import sun.misc.Unsafe;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class LinkedStorageBackpackWrapperTest {
	private static BackpackItem testBackpack;
	private static BackpackItem goldBackpack;
	private static BackpackItem diamondBackpack;

	@BeforeAll
	static void bootstrapRegistries() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		loadDefaultConfig();
		bootstrapPlayerCloneListenerList();
		testBackpack = registerBackpack("backpack", 36, 4);
		goldBackpack = registerBackpack("linked_storage_test_gold_backpack", 54, 6);
		diamondBackpack = registerBackpack("linked_storage_test_diamond_backpack", 72, 8);
	}

	private static void loadDefaultConfig() {
		if (!Config.SERVER_SPEC.isLoaded()) {
			CommentedConfig config = CommentedConfig.inMemory();
			Config.SERVER_SPEC.correct(config);
			Config.SERVER_SPEC.acceptConfig(config);
		}
	}

	@SuppressWarnings("unchecked")
	private static void bootstrapPlayerCloneListenerList() throws ReflectiveOperationException {
		Field listenersField = EventListenerHelper.class.getDeclaredField("listeners");
		listenersField.setAccessible(true);
		LockHelper<Class<?>, ListenerList> listeners = (LockHelper<Class<?>, ListenerList>) listenersField.get(null);
		Field mapField = LockHelper.class.getDeclaredField("map");
		mapField.setAccessible(true);
		Map<Class<?>, ListenerList> listenerLists = (Map<Class<?>, ListenerList>) mapField.get(listeners);
		listenerLists.putIfAbsent(PlayerEvent.Clone.class, new ListenerList(EventListenerHelper.getListenerList(Event.class)));
	}

	@Test
	void virtualHostOwnsContentsAndNeverCreatesOrdinaryStorage() {
		TestContents contents = new TestContents();
		ItemStack carrier = backpack();
		BackpackStorage storage = mock(BackpackStorage.class);

		try (MockedStatic<BackpackStorage> backpackStorage = Mockito.mockStatic(BackpackStorage.class)) {
			BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, carrier);
			host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
			host.getSettingsHandler().getGlobalSettingsCategory().setSettingValue(BackpackMainSettingsCategory.ANOTHER_PLAYER_CAN_OPEN, false);
			host.getUpgradeHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));

			assertTrue(contents.contents.contains(InventoryHandler.INVENTORY_TAG));
			assertTrue(contents.contents.contains(BackpackSettingsHandler.SETTINGS_TAG));
			assertTrue(contents.contents.contains(UpgradeHandler.UPGRADE_INVENTORY_TAG));
			assertTrue(contents.dirtyCount > 0);
			assertEquals(contents.groupId, host.getContentsUuid().orElseThrow());
			assertFalse(carrier.hasTag() && carrier.getTag().hasUUID(BackpackWrapper.CONTENTS_UUID_TAG));
			Mockito.verifyNoInteractions(storage);
			backpackStorage.verifyNoInteractions();
		}
	}

	@Test
	void linkedContentsRebindsInventoryAndUpgrades() {
		TestContents contents = new TestContents();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, backpack());
		InventoryHandler inventory = host.getInventoryHandler();
		BackpackSettingsHandler settings = host.getSettingsHandler();
		UpgradeHandler upgrades = host.getUpgradeHandler();

		contents.setContents(new CompoundTag());
		host.onLinkedStorageContentsChanged();

		assertNotSame(inventory, host.getInventoryHandler());
		assertNotSame(settings, host.getSettingsHandler());
		assertNotSame(upgrades, host.getUpgradeHandler());
	}

	@Test
	void virtualHostRejectsNonBackpackCarrier() {
		assertThrows(IllegalArgumentException.class, () -> new BackpackLinkedStorageHostWrapper(new TestContents(), new ItemStack(Items.STICK)));
	}

	@Test
	void facadeDelegatesCanonicalStateAndKeepsEndpointPresentationPhysical() {
		TestContents contents = new TestContents();
		ItemStack primary = backpack();
		primary.setHoverName(Component.literal("Primary Backpack"));
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, primary);
		BackpackWrapper physical = new BackpackWrapper(backpack());
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(physical, host);
		AtomicInteger changes = new AtomicInteger();

		facade.setContentsChangeHandler(changes::incrementAndGet);
		facade.setColors(0x112233, 0x445566);
		facade.setOpenTabId(4);
		facade.setSlotNumbers(48, 5);
		LinkedStorageEndpointData endpoint = new LinkedStorageEndpointData(contents.groupId, UUID.randomUUID());
		LinkedStorageStackData.setEndpoint(physical.getBackpack(), endpoint);
		facade.setContentsUuid(UUID.randomUUID());
		facade.removeContentsUUIDTag();
		facade.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));

		assertSame(host.getInventoryHandler(), facade.getInventoryHandler());
		assertSame(host.getSettingsHandler(), facade.getSettingsHandler());
		assertSame(host.getUpgradeHandler(), facade.getUpgradeHandler());
		assertSame(physical.getRenderInfo(), facade.getRenderInfo());
		assertEquals("Primary Backpack", facade.getDisplayName().getString());
		assertEquals(0x112233, facade.getMainColor());
		assertEquals(0x445566, facade.getAccentColor());
		assertEquals(4, facade.getOpenTabId().orElseThrow());
		assertTrue(host.getOpenTabId().isEmpty());
		assertEquals(48, host.getInventoryHandler().getSlots());
		assertEquals(48, host.getBackpack().getTag().getInt("inventorySlots"));
		assertEquals(36, physical.getBackpack().getTag().getInt("inventorySlots"));
		assertEquals(contents.groupId, facade.getContentsUuid().orElseThrow());
		assertEquals(endpoint, LinkedStorageStackData.getEndpoint(facade.getBackpack()));
		assertTrue(facade.getInventoryHandler().getStackInSlot(0).is(Items.DIAMOND));
		assertFalse(physical.getBackpack().getTag().hasUUID(BackpackWrapper.CONTENTS_UUID_TAG));
		assertEquals(2, changes.get());
	}

	@Test
	void virtualCarrierChangeRefreshesFacadeTitle() {
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContents(), backpack());
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);
		ItemStack renamed = backpack();
		renamed.setHoverName(Component.literal("Renamed Primary"));

		host.onVirtualCarrierChanged(renamed.save(new CompoundTag()));

		assertEquals("Renamed Primary", facade.getDisplayName().getString());
	}

	@Test
	void canonicalRefreshIsFacadeLocalAndOnlyNotifiesProjectionChanges() {
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContents(), backpack());
		LinkedStorageBackpackWrapper first = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);
		LinkedStorageBackpackWrapper second = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);
		AtomicInteger firstCache = new AtomicInteger();
		AtomicInteger secondCache = new AtomicInteger();
		AtomicInteger projections = new AtomicInteger();
		first.setInventorySlotChangeHandler(firstCache::incrementAndGet);
		second.setInventorySlotChangeHandler(secondCache::incrementAndGet);
		first.setCanonicalContentsChangedHandler(projections::incrementAndGet);

		first.onCanonicalContentsChanged();
		assertEquals(1, firstCache.get());
		assertEquals(0, secondCache.get());
		projections.set(0);
		host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
		first.onCanonicalContentsChanged();

		assertEquals(0, projections.get());
		assertEquals(2, firstCache.get());
		second.onCanonicalContentsChanged();
		assertEquals(1, secondCache.get());
		assertEquals(2, firstCache.get());

		host.getRenderInfo().setBatteryRenderInfo(new IRenderedBatteryUpgrade.BatteryRenderInfo(0.5F));
		first.onCanonicalContentsChanged();
		assertEquals(1, projections.get());
	}

	@Test
	void capturedEndpointAndCloseLifecycleBelongToFacade() {
		ItemStack physicalStack = backpack();
		LinkedStorageEndpointData original = new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID());
		LinkedStorageEndpointData replacement = new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID());
		LinkedStorageStackData.setEndpoint(physicalStack, original);
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(physicalStack),
				new BackpackLinkedStorageHostWrapper(new TestContents(), backpack()));
		AtomicInteger unsubscribes = new AtomicInteger();
		facade.setGroupChangeSubscription(unsubscribes::incrementAndGet);

		LinkedStorageStackData.setEndpoint(physicalStack, replacement);
		facade.close();
		facade.close();

		assertTrue(facade.hasEndpoint(original));
		assertFalse(facade.hasEndpoint(replacement));
		assertEquals(1, unsubscribes.get());
	}

	@Test
	void columnsAreCanonicalAndProjectToEveryFacade() {
		TestContents contents = new TestContents();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, backpack());
		LinkedStorageBackpackWrapper first = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);
		LinkedStorageBackpackWrapper second = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);
		int initialSlots = host.getInventoryHandler().getSlots();

		first.setColumnsTaken(3, true);
		first.onContentsNbtUpdated();
		first.onCanonicalContentsChanged();
		second.onCanonicalContentsChanged();

		assertEquals(3, host.getColumnsTaken());
		assertEquals(3, contents.columnsTaken);
		assertEquals(3, first.getColumnsTaken());
		assertEquals(3, second.getColumnsTaken());
		assertEquals(initialSlots - host.getNumberOfSlotRows() * 3, host.getInventoryHandler().getSlots());
		assertEquals(3, first.getBackpack().getTag().getInt("columnsTaken"));
		assertEquals(3, second.getBackpack().getTag().getInt("columnsTaken"));
	}

	@Test
	void cloneClearsEndpointIdentity() {
		ItemStack stack = backpack();
		LinkedStorageEndpointData endpoint = new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID());
		LinkedStorageStackData.setEndpoint(stack, endpoint);
		BackpackWrapper physical = mock(BackpackWrapper.class);
		when(physical.getBackpack()).thenReturn(stack);
		when(physical.getRenderInfo()).thenReturn(mock(BackpackRenderInfo.class));
		ItemStack stackCopy = stack.copy();
		when(physical.cloneBackpack()).thenReturn(stackCopy);
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(physical, mock(IBackpackWrapper.class));

		ItemStack clone = facade.cloneBackpack();

		assertEquals(LinkedStorageEndpointStackState.UNLINKED, LinkedStorageStackLifecycle.classifyEndpoint(clone));
		assertNull(LinkedStorageStackData.getEndpoint(clone));
		assertEquals(endpoint, LinkedStorageStackData.getEndpoint(stack));
	}

	@Test
	void canonicalRenderDataProjectsToPhysicalStackAndDisplayReadsIt() {
		TestContents contents = new TestContents();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, backpack());
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);
		host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
		host.getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class).selectSlot(0);

		facade.onCanonicalContentsChanged();

		assertEquals(host.getRenderInfo().getNbt(), facade.getRenderInfo().getNbt());
		assertTrue(facade.getBackpack().getTag().contains(BackpackRenderInfo.RENDER_INFO_TAG));
		assertTrue(contents.renderDirtyCount > 0);
	}

	@Test
	void clientCacheReplacesRootsButPreviouslyReadContentsRemainStable() {
		UUID groupId = UUID.randomUUID();
		CompoundTag root = new CompoundTag();
		root.putString("current", "value");
		ClientLinkedStorageContents.clear();
		ClientLinkedStorageContents.updateContents(groupId, 1, root, Component.literal("Main Backpack"), 36, 4, 2);
		ILinkedStorageContents snapshot = ClientLinkedStorageContents.getContents(groupId).orElseThrow();

		assertEquals("value", snapshot.getContents().getString("current"));
		assertEquals("Main Backpack", ClientLinkedStorageContents.getGroupName(groupId).orElseThrow().getString());
		ClientLinkedStorageContents.clear();
		assertEquals("value", snapshot.getContents().getString("current"));
		assertEquals(2, snapshot.getColumnsTaken());
	}

	@Test
	void backpackWrapperReportsPrimaryAndSecondaryEndpointRoles() {
		ItemStack endpoint = backpack();
		LinkedStorageEndpointData data = new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID());
		LinkedStorageStackData.setEndpoint(endpoint, data);
		BackpackWrapper wrapper = new BackpackWrapper(endpoint);

		LinkedStorageStackData.setPrimaryEndpoint(endpoint, true);
		assertTrue(BackpackItem.shouldRenderUpgradeActivity(endpoint));
		assertEquals(LinkedStorageEndpointRole.PRIMARY, wrapper.getLinkedStorageEndpointRole().orElseThrow());

		LinkedStorageStackData.setPrimaryEndpoint(endpoint, false);
		assertFalse(BackpackItem.shouldRenderUpgradeActivity(endpoint));
		assertEquals(LinkedStorageEndpointRole.SECONDARY, wrapper.getLinkedStorageEndpointRole().orElseThrow());
		assertTrue(BackpackItem.shouldRenderUpgradeActivity(backpack()));
	}

	@Test
	void backpackContainerRemainsContextAware() {
		assertTrue(IContextAwareContainer.class.isAssignableFrom(BackpackContainer.class));
	}

	@Test
	void getRenderInfoMarksOnlyTheCanonicalBindingRenderDirty() {
		TestContents contents = new TestContents();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, backpack());

		host.getRenderInfo().setBatteryRenderInfo(new IRenderedBatteryUpgrade.BatteryRenderInfo(0.5F));

		assertEquals(0, contents.dirtyCount);
		assertEquals(1, contents.renderDirtyCount);
	}

	@Test
	void compatibilityMatrixRejectsOnlyExistingInventoryOrUpgradeContents() {
		ServerLevel level = mock(ServerLevel.class);
		BackpackStorage storage = mock(BackpackStorage.class);
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		LinkedStorageHostDescriptor descriptor = new LinkedStorageHostDescriptor(adapter.factoryId(), new CompoundTag());
		UUID contentsId = UUID.randomUUID();
		ItemStack endpoint = backpack();

		try (MockedStatic<BackpackStorage> backpackStorage = Mockito.mockStatic(BackpackStorage.class)) {
			backpackStorage.when(() -> BackpackStorage.get(level)).thenReturn(storage);
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			endpoint.getOrCreateTag().putUUID(BackpackWrapper.CONTENTS_UUID_TAG, contentsId);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.empty());
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(new CompoundTag()));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(rootWithEmptyItemData(InventoryHandler.INVENTORY_TAG)));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(rootWithEmptyItemData(UpgradeHandler.UPGRADE_INVENTORY_TAG)));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(rootWithNonItemData(BackpackSettingsHandler.SETTINGS_TAG)));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(rootWithNonItemData("futureContents")));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(rootWithNonItemData("partitioner")));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(rootWithItemData(InventoryHandler.INVENTORY_TAG)));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.HAS_CONTENTS);
			when(storage.getBackpackContents(contentsId)).thenReturn(Optional.of(rootWithItemData(UpgradeHandler.UPGRADE_INVENTORY_TAG)));
			assertCompatibility(adapter, level, endpoint, descriptor, ILinkedStorageEndpointAdapter.Compatibility.HAS_CONTENTS);
			Mockito.verify(storage, Mockito.never()).removeBackpackContents(contentsId);
		}
	}

	@Test
	void copyCanonicalContentsCopiesTheExactOrdinaryRootWithoutRemovingItsPhysicalUuid() {
		ServerLevel level = mock(ServerLevel.class);
		BackpackStorage storage = mock(BackpackStorage.class);
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		UUID contentsId = UUID.randomUUID();
		CompoundTag root = new CompoundTag();
		root.put(InventoryHandler.INVENTORY_TAG, rootWithItemData(InventoryHandler.INVENTORY_TAG).getCompound(InventoryHandler.INVENTORY_TAG));
		root.put(BackpackSettingsHandler.SETTINGS_TAG, new CompoundTag());
		root.putString("futureContents", "preserved");
		ItemStack endpoint = backpack();
		endpoint.getOrCreateTag().putUUID(BackpackWrapper.CONTENTS_UUID_TAG, contentsId);

		try (MockedStatic<BackpackStorage> backpackStorage = Mockito.mockStatic(BackpackStorage.class)) {
			backpackStorage.when(() -> BackpackStorage.get()).thenReturn(storage);
			when(storage.getOrCreateBackpackContents(contentsId)).thenReturn(root);

			CompoundTag copied = adapter.copyCanonicalContents(level, endpoint);

			assertEquals(root, copied);
			assertNotSame(root, copied);
			assertEquals(contentsId, endpoint.getTag().getUUID(BackpackWrapper.CONTENTS_UUID_TAG));
			Mockito.verify(storage, Mockito.never()).removeBackpackContents(contentsId);
		}
	}

	@Test
	void createHostDescriptorMigratesOnlyCanonicalBackpackState() {
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		ItemStack source = backpack(goldBackpack);
		source.getOrCreateTag().putUUID(BackpackWrapper.CONTENTS_UUID_TAG, UUID.randomUUID());
		source.getTag().putInt("inventorySlots", 54);
		source.getTag().putInt("upgradeSlots", 6);
		BackpackItem.setColors(source, 0x112233, 0x445566);
		LinkedStorageStackData.setEndpoint(source, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		LinkedStorageStackData.setPrimaryEndpoint(source, true);

		ItemStack carrier = ItemStack.of(adapter.createHostDescriptor(mock(ServerLevel.class), source).virtualCarrier());

		assertSame(goldBackpack, carrier.getItem());
		assertEquals(54, carrier.getTag().getInt("inventorySlots"));
		assertEquals(6, carrier.getTag().getInt("upgradeSlots"));
		assertEquals(0x112233, BackpackItem.getMainColor(carrier));
		assertEquals(0x445566, BackpackItem.getAccentColor(carrier));
		assertFalse(carrier.getTag().hasUUID(BackpackWrapper.CONTENTS_UUID_TAG));
		assertEquals(LinkedStorageEndpointStackState.UNLINKED, LinkedStorageStackLifecycle.classifyEndpoint(carrier));
	}

	@Test
	void synchronizeRenderProjectionProjectsCanonicalRenderDataOnlyWhenRevisionChanges() {
		UUID groupId = UUID.randomUUID();
		ItemStack endpoint = backpack();
		LinkedStorageStackData.setEndpoint(endpoint, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		LinkedStorageStackData.setRenderRevision(endpoint, -1L);
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContents(), backpack());
		host.getRenderInfo().setBatteryRenderInfo(new IRenderedBatteryUpgrade.BatteryRenderInfo(0.5F));
		host.getRenderInfo().getNbt().putString("canonicalPayload", "exact");
		ServerLevel level = mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = mock(LinkedStorageGroupManager.class);

		try (MockedStatic<LinkedStorageGroupsSavedData> groups = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groups.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);
			when(savedData.manager()).thenReturn(manager);
			when(manager.getRenderRevision(groupId)).thenReturn(0L, 0L, 1L);
			when(manager.resolveVirtualHost(Mockito.any(LinkedStorageEndpointData.class), Mockito.eq(false))).thenReturn(Optional.of(host));

			assertTrue(BackpackLinkedStorageResolver.synchronizeRenderProjection(level, endpoint));
			assertEquals(host.getRenderInfo().getNbt(), endpoint.getTag().getCompound("renderInfo"));
			host.getRenderInfo().getNbt().putString("revisionPayload", "not-yet-revised");
			assertFalse(BackpackLinkedStorageResolver.synchronizeRenderProjection(level, endpoint));
			assertTrue(BackpackLinkedStorageResolver.synchronizeRenderProjection(level, endpoint));
			assertEquals("not-yet-revised", endpoint.getTag().getCompound("renderInfo").getString("revisionPayload"));
			assertEquals(1L, LinkedStorageStackData.getRenderRevision(endpoint));
		}
	}

	@Test
	void resolveUsesClientSnapshotProfileInsteadOfSecondaryTierSlotCounts() {
		UUID groupId = UUID.randomUUID();
		ItemStack endpoint = backpack();
		LinkedStorageStackData.setEndpoint(endpoint, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		ClientLinkedStorageContents.clear();
		ClientLinkedStorageContents.updateContents(groupId, 1, new CompoundTag(), Component.empty(), 48, 5, 2);

		IBackpackWrapper resolved = BackpackLinkedStorageResolver.resolve(mock(Level.class), endpoint).orElseThrow();

		assertEquals(48 - 2 * resolved.getNumberOfSlotRows(), resolved.getInventoryHandler().getSlots());
		assertEquals(5, resolved.getUpgradeHandler().getSlots());
		ClientLinkedStorageContents.clear();
	}

	@Test
	void getRenderInfoPromotesEndpointChangesToCanonicalHost() {
		TestContents contents = new TestContents();
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(contents, backpack());
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);

		facade.getRenderInfo().setTankRenderInfo(TankPosition.LEFT, new IRenderedTankUpgrade.TankRenderInfo());

		assertTrue(host.getRenderInfo().getTankRenderInfos().containsKey(TankPosition.LEFT));
		assertEquals(1, contents.renderDirtyCount);
	}

	@Test
	void onCanonicalContentsChangedProjectsPhysicalRenderReaders() {
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContents(), backpack());
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(backpack()), host);
		host.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.DIAMOND));
		host.getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class).selectSlot(0);
		host.getRenderInfo().setTankRenderInfo(TankPosition.LEFT, new IRenderedTankUpgrade.TankRenderInfo());
		host.getRenderInfo().getNbt().putString("readerPayload", "physical-stack");

		facade.onCanonicalContentsChanged();
		BackpackRenderInfo physicalRenderInfo = new BackpackRenderInfo(facade.getBackpack(), () -> () -> {
		});

		assertEquals(1, physicalRenderInfo.getItemDisplayRenderInfo().getDisplayItems().size());
		assertTrue(physicalRenderInfo.getTankRenderInfos().containsKey(TankPosition.LEFT));
		assertEquals(host.getRenderInfo().getNbt(), physicalRenderInfo.getNbt());
		assertEquals("physical-stack", facade.getBackpack().getTag().getCompound("renderInfo").getString("readerPayload"));
	}

	@Test
	void onEndpointLinkedClosesOnlyMenusShowingTheLinkedStack() {
		ServerLevel level = mock(ServerLevel.class);
		MinecraftServer server = mock(MinecraftServer.class);
		PlayerList playerList = mock(PlayerList.class);
		ServerPlayer linkedPlayer = mock(ServerPlayer.class);
		ServerPlayer unrelatedPlayer = mock(ServerPlayer.class);
		ItemStack endpoint = backpack();
		IContextAwareContainer linkedContainer = createContextAwareContainer(linkedPlayer, endpoint);
		IContextAwareContainer unrelatedContainer = createContextAwareContainer(unrelatedPlayer, backpack());

		when(level.getServer()).thenReturn(server);
		when(server.getPlayerList()).thenReturn(playerList);
		when(playerList.getPlayers()).thenReturn(List.of(linkedPlayer, unrelatedPlayer));
		when(linkedPlayer.serverLevel()).thenReturn(level);
		when(unrelatedPlayer.serverLevel()).thenReturn(level);
		linkedPlayer.containerMenu = (AbstractContainerMenu) linkedContainer;
		unrelatedPlayer.containerMenu = (AbstractContainerMenu) unrelatedContainer;

		new BackpackLinkedStorageEndpointAdapter().onEndpointLinked(level, endpoint);

		Mockito.verify(linkedPlayer).closeContainer();
		Mockito.verify(unrelatedPlayer, Mockito.never()).closeContainer();
	}

	@Test
	void bindEndpointUsesPrimaryGroupStorageSizeForDifferentTierEndpoint() {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = backpack(goldBackpack);
		primary.getOrCreateTag().putInt("inventorySlots", 54);
		primary.getOrCreateTag().putInt("upgradeSlots", 6);
		ItemStack secondary = backpack();
		BackpackLinkedStorageEndpointAdapter adapter = new BackpackLinkedStorageEndpointAdapter();
		ServerLevel level = mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = mock(LinkedStorageGroupManager.class);
		LinkedStorageHostDescriptor descriptor = new LinkedStorageHostDescriptor(adapter.factoryId(), new CompoundTag());

		when(savedData.manager()).thenReturn(manager);
		when(manager.getHostDescriptor(groupId)).thenReturn(Optional.of(descriptor));
		when(manager.isPrimaryEndpoint(Mockito.eq(groupId), Mockito.any())).thenReturn(false);
		descriptor = new LinkedStorageHostDescriptor(adapter.factoryId(), primary.save(new CompoundTag()));
		when(manager.getHostDescriptor(groupId)).thenReturn(Optional.of(descriptor));
		try (MockedStatic<LinkedStorageGroupsSavedData> groups = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groups.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);
			adapter.bindEndpoint(level, secondary, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		}

		assertEquals(54, secondary.getTag().getInt("inventorySlots"));
		assertEquals(6, secondary.getTag().getInt("upgradeSlots"));
		assertEquals(54, new BackpackLinkedStorageHostWrapper(new TestContents(groupId), secondary).getInventoryHandler().getSlots());
	}

	@Test
	void matchesAllowsTierUpgradeRecipesOnlyForPrimaryEndpoints() {
		ItemStack endpoint = backpack();
		LinkedStorageStackData.setEndpoint(endpoint, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		CraftingContainer crafting = mock(CraftingContainer.class);
		Ingredient ingredient = mock(Ingredient.class);
		BackpackUpgradeRecipe recipe = new BackpackUpgradeRecipe(new ShapedRecipe(new net.minecraft.resources.ResourceLocation("test", "upgrade"), "",
				CraftingBookCategory.MISC, 1, 1, NonNullList.withSize(1, ingredient), backpack()));

		when(crafting.getWidth()).thenReturn(1);
		when(crafting.getHeight()).thenReturn(1);
		when(crafting.getContainerSize()).thenReturn(1);
		when(crafting.getItem(0)).thenReturn(endpoint);
		when(ingredient.test(endpoint)).thenReturn(true);

		LinkedStorageStackData.setPrimaryEndpoint(endpoint, false);
		assertFalse(recipe.matches(crafting, null));

		LinkedStorageStackData.setPrimaryEndpoint(endpoint, true);
		assertTrue(recipe.matches(crafting, null));
	}

	@Test
	void completePrimaryTierUpgradeUpdatesCanonicalCarrierProfile() {
		UUID groupId = UUID.randomUUID();
		UUID endpointId = UUID.randomUUID();
		ItemStack source = backpack();
		LinkedStorageStackData.setEndpoint(source, new LinkedStorageEndpointData(groupId, endpointId));
		ItemStack result = backpack(goldBackpack);
		result.setTag(source.getTag().copy());
		ServerLevel level = mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = mock(LinkedStorageGroupManager.class);

		when(savedData.manager()).thenReturn(manager);
		when(manager.isPrimaryEndpoint(groupId, endpointId)).thenReturn(true);
		when(manager.updatePrimaryHostDescriptor(Mockito.eq(groupId), Mockito.eq(endpointId), Mockito.any())).thenReturn(true);
		try (MockedStatic<LinkedStorageGroupsSavedData> groups = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groups.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);

			assertTrue(BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(level, result, new SimpleContainer(source)));
		}

		assertEquals(54, result.getTag().getInt("inventorySlots"));
		assertEquals(6, result.getTag().getInt("upgradeSlots"));
		org.mockito.ArgumentCaptor<LinkedStorageHostDescriptor> descriptorCaptor = org.mockito.ArgumentCaptor.forClass(LinkedStorageHostDescriptor.class);
		Mockito.verify(manager).updatePrimaryHostDescriptor(Mockito.eq(groupId), Mockito.eq(endpointId), descriptorCaptor.capture());
		ItemStack upgradedCarrier = ItemStack.of(descriptorCaptor.getValue().virtualCarrier());
		assertSame(goldBackpack, upgradedCarrier.getItem());
		assertEquals(54, upgradedCarrier.getTag().getInt("inventorySlots"));
		assertEquals(6, upgradedCarrier.getTag().getInt("upgradeSlots"));
	}

	@Test
	void resolveUsesServerCanonicalHostForEndpoint() {
		UUID groupId = UUID.randomUUID();
		ItemStack endpoint = backpack();
		LinkedStorageStackData.setEndpoint(endpoint, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContents(groupId), backpack());
		ServerLevel level = mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = mock(LinkedStorageGroupManager.class);

		when(savedData.manager()).thenReturn(manager);
		when(manager.isEndpointMember(Mockito.eq(groupId), Mockito.any())).thenReturn(true);
		when(manager.resolveVirtualHost(groupId)).thenReturn(Optional.of(host));
		when(manager.subscribeToGroupChanges(Mockito.eq(groupId), Mockito.any())).thenReturn(() -> {
		});
		try (MockedStatic<LinkedStorageGroupsSavedData> groups = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groups.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);

			IBackpackWrapper resolved = BackpackLinkedStorageResolver.resolve(level, endpoint).orElseThrow();

			assertSame(host.getInventoryHandler(), resolved.getInventoryHandler());
			assertEquals(groupId, resolved.getContentsUuid().orElseThrow());
			Mockito.verify(manager).subscribeToGroupChanges(Mockito.eq(groupId), Mockito.any());
		}
	}

	@Test
	void resolveCanonicalHostResolvesSecondaryEndpointForCapabilitiesButNotGlobalUpgradeProcessing() {
		UUID groupId = UUID.randomUUID();
		UUID endpointId = UUID.randomUUID();
		ItemStack endpoint = backpack();
		LinkedStorageStackData.setEndpoint(endpoint, new LinkedStorageEndpointData(groupId, endpointId));
		BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(new TestContents(groupId), backpack());
		ServerLevel level = mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = mock(LinkedStorageGroupManager.class);

		when(savedData.manager()).thenReturn(manager);
		when(manager.isEndpointMember(groupId, endpointId)).thenReturn(true);
		when(manager.isPrimaryEndpoint(groupId, endpointId)).thenReturn(false);
		when(manager.resolveVirtualHost(groupId)).thenReturn(Optional.of(host));
		try (MockedStatic<LinkedStorageGroupsSavedData> groups = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groups.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);

			assertSame(host, BackpackLinkedStorageResolver.resolveCanonicalHost(level, endpoint).orElseThrow());
			assertTrue(BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, endpoint).isEmpty());

			assertSame(IBackpackWrapper.Noop.INSTANCE, BackpackLinkedStorageResolver.resolveForGlobalUpgradeProcessing(level, endpoint));
		}
	}

	@Test
	void inventoryTickDispatchesOnlyPrimaryLinkedEndpoint() {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = linkedEndpoint(groupId);
		ItemStack secondary = linkedEndpoint(groupId);
		ServerLevel level = mock(ServerLevel.class);
		Player player = mock(Player.class);
		BlockPos playerPos = new BlockPos(4, 70, 9);
		IBackpackWrapper canonicalHost = mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		ITickableUpgrade tickableUpgrade = mock(ITickableUpgrade.class);

		when(player.isSpectator()).thenReturn(false);
		when(player.isDeadOrDying()).thenReturn(false);
		when(player.level()).thenReturn(level);
		when(player.blockPosition()).thenReturn(playerPos);
		when(canonicalHost.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getWrappersThatImplement(ITickableUpgrade.class)).thenReturn(List.of(tickableUpgrade));
		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.synchronizeRenderProjection(level, primary)).thenReturn(false);
			resolver.when(() -> BackpackLinkedStorageResolver.synchronizeRenderProjection(level, secondary)).thenReturn(false);
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, primary)).thenReturn(Optional.of(canonicalHost));
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, secondary)).thenReturn(Optional.empty());

			testBackpack.inventoryTick(primary, level, player, -1, false);
			testBackpack.inventoryTick(secondary, level, player, -1, false);
		}

		Mockito.verify(tickableUpgrade).tick(player, level, playerPos);
		Mockito.verifyNoMoreInteractions(tickableUpgrade);
	}

	@Test
	void inventoryTickSkipsNonPlayersSpectatorsDeadPlayersAndUnwornPrimaryEndpoints() {
		ItemStack endpoint = linkedEndpoint(UUID.randomUUID());
		ServerLevel level = mock(ServerLevel.class);
		Player spectator = mock(Player.class);
		Player deadPlayer = mock(Player.class);
		Player inventoryPlayer = mock(Player.class);
		IBackpackWrapper canonicalHost = mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		ITickableUpgrade tickableUpgrade = mock(ITickableUpgrade.class);
		boolean onlyWorn = Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get();

		when(spectator.isSpectator()).thenReturn(true);
		when(deadPlayer.isSpectator()).thenReturn(false);
		when(deadPlayer.isDeadOrDying()).thenReturn(true);
		when(inventoryPlayer.isSpectator()).thenReturn(false);
		when(inventoryPlayer.isDeadOrDying()).thenReturn(false);
		when(canonicalHost.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getWrappersThatImplement(ITickableUpgrade.class)).thenReturn(List.of(tickableUpgrade));
		try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.synchronizeRenderProjection(level, endpoint)).thenReturn(false);
			resolver.when(() -> BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, endpoint)).thenReturn(Optional.of(canonicalHost));

			testBackpack.inventoryTick(endpoint, level, mock(Entity.class), -1, false);
			testBackpack.inventoryTick(endpoint, level, spectator, -1, false);
			testBackpack.inventoryTick(endpoint, level, deadPlayer, -1, false);
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(true);
			testBackpack.inventoryTick(endpoint, level, inventoryPlayer, 0, false);
		} finally {
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(onlyWorn);
		}

		Mockito.verify(tickableUpgrade, Mockito.never()).tick(Mockito.any(), Mockito.any(), Mockito.any());
	}

	@Test
	void serverTickDispatchesOnlyPrimaryLinkedEndpoint() throws ReflectiveOperationException {
		UUID groupId = UUID.randomUUID();
		ItemStack primary = linkedEndpoint(groupId);
		ItemStack secondary = linkedEndpoint(groupId);
		ServerLevel level = mock(ServerLevel.class);
		BlockPos primaryPos = new BlockPos(1, 64, 1);
		BlockPos secondaryPos = new BlockPos(2, 64, 1);
		IBackpackWrapper canonicalHost = mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		ITickableUpgrade tickableUpgrade = mock(ITickableUpgrade.class);
		BackpackBlockEntity primaryBlock = uninitializedBackpackBlockEntity();
		BackpackBlockEntity secondaryBlock = uninitializedBackpackBlockEntity();

		when(canonicalHost.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getWrappersThatImplement(ITickableUpgrade.class)).thenReturn(List.of(tickableUpgrade));
		setBackpackWrapper(primaryBlock, wrapperWithBackpack(primary));
		setBackpackWrapper(secondaryBlock, wrapperWithBackpack(secondary));
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
	void playUsesProvidedPrimaryAnchorInsteadOfInitiatingEndpoint() {
		ServerLevel initiatingLevel = mock(ServerLevel.class);
		ServerLevel primaryLevel = mock(ServerLevel.class);
		BlockPos initiatingPos = new BlockPos(1, 64, 1);
		BlockPos primaryPos = new BlockPos(9, 70, -3);
		UUID storageUuid = UUID.randomUUID();
		IStorageWrapper storageWrapper = mock(IStorageWrapper.class, Mockito.withSettings().extraInterfaces(IJukeboxPlaybackLocationProvider.class));
		IJukeboxPlaybackLocationProvider playbackLocationProvider = (IJukeboxPlaybackLocationProvider) storageWrapper;
		RenderInfo renderInfo = mock(RenderInfo.class);

		when(storageWrapper.getContentsUuid()).thenReturn(Optional.of(storageUuid));
		when(storageWrapper.getRenderInfo()).thenReturn(renderInfo);
		when(playbackLocationProvider.getJukeboxPlaybackLocation(initiatingLevel))
				.thenReturn(Optional.of(JukeboxPlaybackLocation.forBlock(primaryLevel, primaryPos)));
		TestJukeboxUpgradeWrapper wrapper = new TestJukeboxUpgradeWrapper(storageWrapper, jukeboxUpgrade());
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
			((net.minecraftforge.items.ItemStackHandler) wrapper.getDiscInventory()).setStackInSlot(0, new ItemStack(Items.STICK));
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
		ItemStack endpoint = backpack();
		LinkedStorageStackData.setEndpoint(endpoint, new LinkedStorageEndpointData(groupId, endpointId));
		ServerLevel level = mock(ServerLevel.class);
		LinkedStorageGroupsSavedData savedData = mock(LinkedStorageGroupsSavedData.class);
		LinkedStorageGroupManager manager = mock(LinkedStorageGroupManager.class);

		when(savedData.manager()).thenReturn(manager);
		when(manager.isPrimaryEndpoint(groupId, endpointId)).thenReturn(false);
		try (MockedStatic<LinkedStorageGroupsSavedData> groups = Mockito.mockStatic(LinkedStorageGroupsSavedData.class)) {
			groups.when(() -> LinkedStorageGroupsSavedData.get(level)).thenReturn(savedData);

			assertFalse(LinkedStorageJukeboxPlaybackAnchors.isPrimaryEndpoint(level, endpoint));
		}
	}

	private static CompoundTag rootWithItemData(String inventoryTag) {
		CompoundTag root = new CompoundTag();
		CompoundTag inventory = new CompoundTag();
		ListTag items = new ListTag();
		items.add(new CompoundTag());
		inventory.put("Items", items);
		root.put(inventoryTag, inventory);
		return root;
	}

	private static CompoundTag rootWithEmptyItemData(String inventoryTag) {
		CompoundTag root = new CompoundTag();
		CompoundTag inventory = new CompoundTag();
		inventory.put("Items", new ListTag());
		root.put(inventoryTag, inventory);
		return root;
	}

	private static CompoundTag rootWithNonItemData(String key) {
		CompoundTag root = new CompoundTag();
		root.put(key, new CompoundTag());
		return root;
	}

	private static void assertCompatibility(BackpackLinkedStorageEndpointAdapter adapter, ServerLevel level, ItemStack endpoint,
			LinkedStorageHostDescriptor descriptor, ILinkedStorageEndpointAdapter.Compatibility expected) {
		assertEquals(expected, adapter.getCompatibility(level, endpoint, descriptor));
		assertEquals(expected == ILinkedStorageEndpointAdapter.Compatibility.COMPATIBLE, adapter.isCompatible(level, endpoint, descriptor));
	}

	private static IContextAwareContainer createContextAwareContainer(ServerPlayer player, ItemStack backpack) {
		AbstractContainerMenu menu = mock(AbstractContainerMenu.class, Mockito.withSettings().extraInterfaces(IContextAwareContainer.class));
		IContextAwareContainer contextAwareContainer = (IContextAwareContainer) menu;
		BackpackContext context = mock(BackpackContext.class);
		IBackpackWrapper wrapper = mock(IBackpackWrapper.class);
		when(contextAwareContainer.getBackpackContext()).thenReturn(context);
		when(context.getBackpackWrapper(player)).thenReturn(wrapper);
		when(wrapper.getBackpack()).thenReturn(backpack);
		return contextAwareContainer;
	}

	private static ItemStack linkedEndpoint(UUID groupId) {
		ItemStack stack = backpack();
		LinkedStorageStackData.setEndpoint(stack, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
		return stack;
	}

	private static IBackpackWrapper wrapperWithBackpack(ItemStack stack) {
		IBackpackWrapper wrapper = mock(IBackpackWrapper.class);
		when(wrapper.getBackpack()).thenReturn(stack);
		return wrapper;
	}

	private static void setBackpackWrapper(BackpackBlockEntity blockEntity, IBackpackWrapper backpackWrapper) throws ReflectiveOperationException {
		Field field = BackpackBlockEntity.class.getDeclaredField("backpackWrapper");
		field.setAccessible(true);
		field.set(blockEntity, backpackWrapper);
	}

	private static BackpackBlockEntity uninitializedBackpackBlockEntity() throws ReflectiveOperationException {
		Field unsafeField = Unsafe.class.getDeclaredField("theUnsafe");
		unsafeField.setAccessible(true);
		return (BackpackBlockEntity) ((Unsafe) unsafeField.get(null)).allocateInstance(BackpackBlockEntity.class);
	}

	private static ItemStack jukeboxUpgrade() {
		JukeboxUpgradeItem upgradeItem = mock(JukeboxUpgradeItem.class);
		when(upgradeItem.getNumberOfSlots()).thenReturn(1);
		ItemStack upgrade = mock(ItemStack.class, delegatesTo(new ItemStack(Items.CHEST)));
		when(upgrade.getItem()).thenReturn(upgradeItem);
		return upgrade;
	}

	private static ItemStack backpack() {
		return backpack(testBackpack);
	}

	private static ItemStack backpack(BackpackItem backpackItem) {
		return new ItemStack(backpackItem);
	}

	private static BackpackItem registerBackpack(String name, int inventorySlots, int upgradeSlots) {
		setItemRegistryFrozen(false);
		BackpackItem backpack = new BackpackItem(() -> inventorySlots, () -> upgradeSlots, () -> null);
		Registry.register(BuiltInRegistries.ITEM, "sophisticatedbackpacks:" + name, backpack);
		setItemRegistryFrozen(true);
		return backpack;
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
					throw new IllegalStateException("Unable to register serialized backpack test fixture", e);
				}
			}
		}
		if (!foundFrozen || !foundLocked) {
			throw new IllegalStateException("Unable to locate the item registry write guards");
		}
	}

	private static class TestJukeboxUpgradeWrapper extends JukeboxUpgradeWrapper {
		private TestJukeboxUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade) {
			super(storageWrapper, upgrade, ignored -> {
			});
		}
	}

	private static class TestContents implements ILinkedStorageContents {
		private final UUID groupId;
		private CompoundTag contents = new CompoundTag();
		private int columnsTaken;
		private int dirtyCount;
		private int renderDirtyCount;

		private TestContents() {
			this(UUID.randomUUID());
		}

		private TestContents(UUID groupId) {
			this.groupId = groupId;
		}

		@Override
		public UUID groupId() {
			return groupId;
		}

		@Override
		public CompoundTag getContents() {
			return contents;
		}

		@Override
		public void setContents(CompoundTag contents) {
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
