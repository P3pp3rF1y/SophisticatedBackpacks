package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import com.electronwill.nightconfig.core.CommentedConfig;
import com.mojang.serialization.DynamicOps;
import net.minecraft.SharedConstants;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.neoforged.fml.config.IConfigSpec;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.StorageWrapperRepository;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.CodecHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InceptionBackpackPersistenceTest {
	private static final RegistryAccess REGISTRY_ACCESS = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
	private static final DynamicOps<Tag> NBT_OPS = REGISTRY_ACCESS.createSerializationContext(NbtOps.INSTANCE);
	private static final Field UPGRADE_STORAGE_WRAPPER_FIELD;

	static {
		try {
			UPGRADE_STORAGE_WRAPPER_FIELD = UpgradeWrapperBase.class.getDeclaredField("storageWrapper");
			UPGRADE_STORAGE_WRAPPER_FIELD.setAccessible(true);
		} catch (NoSuchFieldException e) {
			throw new ExceptionInInitializerError(e);
		}
	}

	private final Set<UUID> storageUuids = new HashSet<>();

	@BeforeAll
	static void setup() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		loadDefaultConfig(Config.SERVER_SPEC);
	}

	@AfterEach
	void cleanup() {
		StorageWrapperRepository.clearCache();
		storageUuids.forEach(BackpackStorage.get()::removeBackpackContents);
		storageUuids.clear();
	}

	@Test
	void getInventoryForUpgradeProcessingPersistsUuidOfNewNestedBackpack() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registryHelper.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				StorageWrapperRepository.clearCache();

				IBackpackWrapper outerBackpack = createBackpack();
				ItemStack inceptionUpgrade = new ItemStack(ModItems.INCEPTION_UPGRADE.get());
				inceptionUpgrade.set(ModDataComponents.INVENTORY_ORDER, InventoryOrder.INCEPTED_FIRST);
				outerBackpack.getUpgradeHandler().setStackInSlot(0, inceptionUpgrade);
				ItemStack innerBackpackStack = new ItemStack(ModItems.BACKPACK.get());
				outerBackpack.getInventoryHandler().setStackInSlot(0, innerBackpackStack);

				outerBackpack.getInventoryForUpgradeProcessing();
				UUID innerContentsUuid = BackpackWrapper.fromStack(innerBackpackStack).getContentsUuid().orElseThrow();
				storageUuids.add(innerContentsUuid);
				ItemStack savedOuterBackpackStack = copyItemStack(outerBackpack.getBackpack());
				StorageWrapperRepository.clearCache();
				IBackpackWrapper reloadedOuterBackpack = BackpackWrapper.fromStack(savedOuterBackpackStack);

				ItemStack persistedInnerBackpackStack = reloadedOuterBackpack.getInventoryHandler().getStackInSlot(0);
				assertEquals(innerContentsUuid, BackpackWrapper.fromStack(persistedInnerBackpackStack).getContentsUuid().orElseThrow());
			}
		});
	}

	@Test
	void nestedBackpackItemRemovalStaysPersistedAfterInceptionCacheExpiryAndNestedInventoryExtraction() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registryHelper.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				StorageWrapperRepository.clearCache();

				IBackpackWrapper outerBackpack = createBackpack();
				IBackpackWrapper innerBackpack = createBackpack();
				innerBackpack.getInventoryHandler().setStackInSlot(0, new ItemStack(Items.FIREWORK_ROCKET, 64));
				innerBackpack.getInventoryHandler().setStackInSlot(1, new ItemStack(Items.DIAMOND));
				innerBackpack.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.ADVANCED_REFILL_UPGRADE.get()));

				ItemStack innerBackpackStack = innerBackpack.getBackpack();
				outerBackpack.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
				outerBackpack.getInventoryHandler().setStackInSlot(0, innerBackpackStack);

				ITrackedContentsItemHandler inceptedInventory = outerBackpack.getInventoryForUpgradeProcessing();
				IBackpackWrapper cachedInnerBackpack = BackpackWrapper.fromStack(innerBackpackStack);
				cachedInnerBackpack.getInventoryForInputOutput();
				RefillUpgradeWrapper cachedRefillWrapper = getRefillWrapper(outerBackpack);
				assertSame(cachedInnerBackpack, getStorageWrapper(cachedRefillWrapper));
				assertSavedItemCount(innerBackpack, Items.DIAMOND, 1);

				StorageWrapperRepository.clearCache();
				IBackpackWrapper openedInnerBackpack = BackpackWrapper.fromStack(outerBackpack.getInventoryHandler().getStackInSlot(0));
				assertNotSame(cachedInnerBackpack, openedInnerBackpack);

				ItemStack extractedDiamond = openedInnerBackpack.getInventoryHandler().extractItem(1, 1, false);
				assertTrue(extractedDiamond.is(Items.DIAMOND));
				assertSavedItemCount(openedInnerBackpack, Items.DIAMOND, 0);

				RefillUpgradeWrapper refreshedRefillWrapper = getRefillWrapper(outerBackpack);
				IStorageWrapper refreshedRefillStorageWrapper = getStorageWrapper(refreshedRefillWrapper);
				assertSame(openedInnerBackpack, refreshedRefillStorageWrapper);
				ItemStack extractedByRefillStorageWrapper = refreshedRefillStorageWrapper.getInventoryForUpgradeProcessing()
						.extractItem(new ItemStack(Items.FIREWORK_ROCKET), false);
				assertEquals(1, extractedByRefillStorageWrapper.getCount());
				assertTrue(extractedByRefillStorageWrapper.is(Items.FIREWORK_ROCKET));
				assertSavedItemCount(openedInnerBackpack, Items.DIAMOND, 0);

				ItemStack extractedRocket = inceptedInventory.extractItem(new ItemStack(Items.FIREWORK_ROCKET), false);
				assertEquals(1, extractedRocket.getCount());
				assertTrue(extractedRocket.is(Items.FIREWORK_ROCKET));
				assertSavedItemCount(openedInnerBackpack, Items.DIAMOND, 0);
			}
		});
	}

	@Test
	void saveInitializedSubBackpacksPersistsUuidAllocatedBeforeHandlerCreation() throws Throwable {
		runOnServerThread(() -> {
			try (MockedStatic<RegistryHelper> registryHelper = Mockito.mockStatic(RegistryHelper.class, Mockito.CALLS_REAL_METHODS)) {
				registryHelper.when(RegistryHelper::getRegistryAccess).thenReturn(Optional.of(REGISTRY_ACCESS));
				StorageWrapperRepository.clearCache();

				IBackpackWrapper outerBackpack = createBackpack();
				ItemStack innerBackpackStack = new ItemStack(ModItems.BACKPACK.get());
				outerBackpack.getInventoryHandler().setStackInSlot(0, innerBackpackStack);
				BackpackWrapper.fromStack(innerBackpackStack).getInventoryHandler();
				UUID innerContentsUuid = BackpackWrapper.fromStack(innerBackpackStack).getContentsUuid().orElseThrow();
				storageUuids.add(innerContentsUuid);
				SubBackpacksHandler subBackpacksHandler = new SubBackpacksHandler(outerBackpack.getInventoryHandler());

				subBackpacksHandler.saveInitializedSubBackpacks();
				ItemStack savedOuterBackpackStack = copyItemStack(outerBackpack.getBackpack());
				StorageWrapperRepository.clearCache();
				IBackpackWrapper reloadedOuterBackpack = BackpackWrapper.fromStack(savedOuterBackpackStack);

				ItemStack persistedInnerBackpackStack = reloadedOuterBackpack.getInventoryHandler().getStackInSlot(0);
				assertEquals(innerContentsUuid, BackpackWrapper.fromStack(persistedInnerBackpackStack).getContentsUuid().orElseThrow());
			}
		});
	}

	private IBackpackWrapper createBackpack() {
		IBackpackWrapper backpack = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
		backpack.getInventoryHandler();
		storageUuids.add(backpack.getContentsUuid().orElseThrow());
		return backpack;
	}

	private static ItemStack copyItemStack(ItemStack stack) {
		return CodecHelper.OVERSIZED_ITEM_STACK_CODEC.parse(NBT_OPS, CodecHelper.OVERSIZED_ITEM_STACK_CODEC.encodeStart(NBT_OPS, stack).getOrThrow())
				.getOrThrow();
	}

	private RefillUpgradeWrapper getRefillWrapper(IBackpackWrapper backpack) {
		List<RefillUpgradeWrapper> refillWrappers = backpack.getUpgradeHandler().getWrappersThatImplement(RefillUpgradeWrapper.class);
		assertEquals(1, refillWrappers.size());
		return refillWrappers.getFirst();
	}

	private static IStorageWrapper getStorageWrapper(RefillUpgradeWrapper wrapper) throws IllegalAccessException {
		return (IStorageWrapper) UPGRADE_STORAGE_WRAPPER_FIELD.get(wrapper);
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

	private void assertSavedItemCount(IBackpackWrapper backpack, Item item, int expectedCount) {
		UUID contentsUuid = backpack.getContentsUuid().orElseThrow();
		CompoundTag inventoryNbt = BackpackStorage.get().getOrCreateBackpackContents(contentsUuid).getCompound(InventoryHandler.INVENTORY_TAG)
				.orElseGet(CompoundTag::new);
		int actualCount = inventoryNbt.getListOrEmpty("Items").stream().map(tag -> CodecHelper.OVERSIZED_ITEM_STACK_CODEC.parse(NBT_OPS, tag).getOrThrow())
				.filter(stack -> stack.is(item)).mapToInt(ItemStack::getCount).sum();

		assertEquals(expectedCount, actualCount);
	}

	// BackpackStorage and the parent-slot save callback use the thread group to select server behavior.
	private static void runOnServerThread(ThrowingRunnable runnable) throws Throwable {
		AtomicReference<Throwable> thrown = new AtomicReference<>();
		Thread thread = new Thread(SidedThreadGroups.SERVER, () -> {
			try {
				runnable.run();
			} catch (Throwable t) {
				thrown.set(t);
			}
		}, "inception-backpack-persistence-test");
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
