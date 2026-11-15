package net.p3pp3rf1y.sophisticatedbackpacks.network;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.NonNullList;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponents;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.fml.config.IConfigSpec;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.inventory.StorageWrapperRepository;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.lang.reflect.Constructor;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

class SyncClientInfoPayloadTest {
	private final Set<UUID> storageUuids = new HashSet<>();

	@BeforeAll
	static void setup() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
		ModItems.BACKPACK.get().builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		loadDefaultConfig(Config.SERVER_SPEC);
	}

	@AfterEach
	void cleanup() {
		StorageWrapperRepository.clearCache();
		storageUuids.forEach(BackpackStorage.get()::removeBackpackContents);
		storageUuids.clear();
	}

	@Test
	void handlePayloadInvalidatesHeldWrapperCachesWhenMenuAndHeldUuidsDifferAfterGeometryChange() throws Throwable {
		runOnServerThread(() -> {
			TestPayloadContext testContext = createPayloadContext();
			WrapperCaches caches = initializeCaches(testContext.heldWrapper());

			SyncClientInfoPayload.handlePayload(new SyncClientInfoPayload(0, new RenderData(), 2), testContext.context());

			assertInvalidated(testContext.heldWrapper(), caches);
			assertHeldWrapper(testContext, 90);
		});
	}

	@Test
	void handlePayloadInvalidatesHeldWrapperCachesWhenMenuAndHeldUuidsDifferWithSameGeometry() throws Throwable {
		runOnServerThread(() -> {
			TestPayloadContext testContext = createPayloadContext();
			WrapperCaches caches = initializeCaches(testContext.heldWrapper());

			SyncClientInfoPayload.handlePayload(new SyncClientInfoPayload(0, new RenderData(), 0), testContext.context());

			assertInvalidated(testContext.heldWrapper(), caches);
			assertHeldWrapper(testContext, 108);
		});
	}

	@Test
	void handlePayloadKeepsHeldWrapperCachesWhenMenuAndHeldUuidsMatchWithSameGeometry() throws Throwable {
		runOnServerThread(() -> {
			TestPayloadContext testContext = createPayloadContext();
			Mockito.when(testContext.menu().getStorageWrapper()).thenReturn(testContext.heldWrapper());
			WrapperCaches caches = initializeCaches(testContext.heldWrapper());

			SyncClientInfoPayload.handlePayload(new SyncClientInfoPayload(0, new RenderData(), 0), testContext.context());

			assertUnchanged(testContext.heldWrapper(), caches);
			assertHeldWrapper(testContext, 108);
		});
	}

	private TestPayloadContext createPayloadContext() {
		ItemStack menuStack = new ItemStack(ModItems.BACKPACK.get());
		IBackpackWrapper menuWrapper = BackpackWrapper.fromStack(menuStack);
		menuWrapper.getInventoryHandler();
		UUID menuUuid = menuWrapper.getContentsUuid().orElseThrow();
		ItemStack heldStack = new ItemStack(ModItems.BACKPACK.get());
		IBackpackWrapper heldWrapper = BackpackWrapper.fromStack(heldStack);
		heldWrapper.getInventoryHandler();
		UUID heldUuid = heldWrapper.getContentsUuid().orElseThrow();
		heldWrapper.setSlotNumbers(108, 5);
		storageUuids.add(menuUuid);
		storageUuids.add(heldUuid);

		Player player = Mockito.mock(Player.class);
		Inventory inventory = Mockito.mock(Inventory.class);
		BackpackContainer menu = Mockito.mock(BackpackContainer.class);
		IPayloadContext context = Mockito.mock(IPayloadContext.class);
		player.containerMenu = menu;
		Mockito.when(context.player()).thenReturn(player);
		Mockito.when(player.getInventory()).thenReturn(inventory);
		NonNullList<ItemStack> nonEquipmentItems = NonNullList.create();
		nonEquipmentItems.add(heldStack);
		Mockito.when(inventory.getNonEquipmentItems()).thenReturn(nonEquipmentItems);
		Mockito.when(inventory.getItem(0)).thenReturn(heldStack);
		Mockito.when(menu.canApplyClientInfo(0)).thenReturn(true);
		Mockito.when(menu.getStorageWrapper()).thenReturn(menuWrapper);

		return new TestPayloadContext(context, menu, heldStack, heldWrapper, heldUuid);
	}

	private static WrapperCaches initializeCaches(IBackpackWrapper wrapper) {
		return new WrapperCaches(wrapper.getInventoryHandler(), wrapper.getSettingsHandler(), wrapper.getUpgradeHandler(), wrapper.getInventoryForInputOutput(),
				wrapper.getInventoryForUpgradeProcessing());
	}

	private static void assertInvalidated(IBackpackWrapper wrapper, WrapperCaches caches) {
		assertNotSame(caches.inventory(), wrapper.getInventoryHandler());
		assertNotSame(caches.settings(), wrapper.getSettingsHandler());
		assertNotSame(caches.upgrades(), wrapper.getUpgradeHandler());
		assertNotSame(caches.inputOutput(), wrapper.getInventoryForInputOutput());
		assertNotSame(caches.upgradeProcessing(), wrapper.getInventoryForUpgradeProcessing());
	}

	private static void assertUnchanged(IBackpackWrapper wrapper, WrapperCaches caches) {
		assertSame(caches.inventory(), wrapper.getInventoryHandler());
		assertSame(caches.settings(), wrapper.getSettingsHandler());
		assertSame(caches.upgrades(), wrapper.getUpgradeHandler());
		assertSame(caches.inputOutput(), wrapper.getInventoryForInputOutput());
		assertSame(caches.upgradeProcessing(), wrapper.getInventoryForUpgradeProcessing());
	}

	private static void assertHeldWrapper(TestPayloadContext testContext, int expectedInventorySize) {
		IBackpackWrapper resolvedHeldWrapper = BackpackWrapper.fromStack(testContext.heldStack());
		assertSame(testContext.heldWrapper(), resolvedHeldWrapper);
		assertEquals(testContext.heldUuid(), resolvedHeldWrapper.getContentsUuid().orElseThrow());
		assertEquals(expectedInventorySize, resolvedHeldWrapper.getInventoryHandler().size());
	}

	private record TestPayloadContext(IPayloadContext context, BackpackContainer menu, ItemStack heldStack, IBackpackWrapper heldWrapper, UUID heldUuid) {
	}

	private record WrapperCaches(Object inventory, Object settings, UpgradeHandler upgrades, Object inputOutput, Object upgradeProcessing) {
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

	private static void runOnServerThread(ThrowingRunnable runnable) throws Throwable {
		AtomicReference<Throwable> thrown = new AtomicReference<>();
		Thread thread = new Thread(SidedThreadGroups.SERVER, () -> {
			try {
				runnable.run();
			} catch (Throwable t) {
				thrown.set(t);
			}
		}, "sync-client-info-payload-test");
		thread.start();
		thread.join();

		if (thrown.get() != null) {
			throw thrown.get();
		}
	}

	@FunctionalInterface
	private interface ThrowingRunnable {
		void run() throws Throwable;
	}
}
