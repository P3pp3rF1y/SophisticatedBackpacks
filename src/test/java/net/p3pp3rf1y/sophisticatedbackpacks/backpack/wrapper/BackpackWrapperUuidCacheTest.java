package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponents;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.network.chat.Component;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.SmithingRecipeInput;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.phys.AABB;
import net.neoforged.fml.config.IConfigSpec;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.neoforged.neoforge.transfer.access.ItemAccess;
import net.neoforged.neoforge.transfer.energy.EnergyHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.UUIDDeduplicator;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.SubBackpacksHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.StorageWrapperRepository;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Constructor;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BackpackWrapperUuidCacheTest {
	private final Set<UUID> storageUuids = new HashSet<>();

	@BeforeAll
	static void setup() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
		ModItems.BACKPACK.get().builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		ModItems.GOLD_BACKPACK.get().builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		ModItems.INCEPTION_UPGRADE.get().builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		ModItems.BATTERY_UPGRADE.get().builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		ModItems.TANK_UPGRADE.get().builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		Items.STICK.builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 64).build());
		Items.EXPERIENCE_BOTTLE.builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 64).build());
		Items.POTION.builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		Items.GLASS_BOTTLE.builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 64).build());
		Fluids.WATER.builtInRegistryHolder().bindComponents(DataComponentMap.EMPTY);
		loadDefaultConfig(Config.SERVER_SPEC);
	}

	@AfterEach
	void cleanup() {
		StorageWrapperRepository.clearCache();
		storageUuids.forEach(BackpackStorage.get()::removeBackpackContents);
		storageUuids.clear();
	}

	@Test
	void fromStackReusesUuidWrapperAndInitializedInventoryHandlerForCopiedStacks() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			InventoryHandler inventoryHandler = wrapper.getInventoryHandler();
			UUID storageUuid = wrapper.getContentsUuid().orElseThrow();
			storageUuids.add(storageUuid);
			long cacheChanges = StorageWrapperRepository.getWrapperCacheChangeCounter();
			RenderData originalRenderData = stack.get(ModCoreDataComponents.RENDER_DATA).copy();
			ItemStack latestCopy = ItemStack.EMPTY;
			boolean reusedCanonicalWrapper = true;
			boolean reusedInventoryHandler = true;
			boolean updatedCopiedStack = true;
			boolean copiedStackHasNoExistingWrapper = true;

			for (int i = 0; i < 1_000; i++) {
				ItemStack copiedStack = stack.copy();
				latestCopy = copiedStack;
				IBackpackWrapper copiedWrapper = BackpackWrapper.fromStack(copiedStack);
				reusedCanonicalWrapper &= wrapper == copiedWrapper;
				reusedInventoryHandler &= inventoryHandler == copiedWrapper.getInventoryHandler();
				updatedCopiedStack &= copiedStack == copiedWrapper.getBackpack();
				copiedStackHasNoExistingWrapper &= BackpackWrapper.fromExistingData(copiedStack).isEmpty();
			}

			wrapper.getRenderDataHandler().setBatteryRenderData(new RenderData.BatteryRenderData(0.5F));
			RenderData sourceRenderData = stack.get(ModCoreDataComponents.RENDER_DATA);
			boolean latestCopyHasBatteryRenderData = latestCopy.get(ModCoreDataComponents.RENDER_DATA).battery().isPresent();
			long finalCacheChanges = StorageWrapperRepository.getWrapperCacheChangeCounter();

			assertTrue(reusedCanonicalWrapper);
			assertTrue(reusedInventoryHandler);
			assertTrue(updatedCopiedStack);
			assertTrue(copiedStackHasNoExistingWrapper);
			assertEquals(originalRenderData, sourceRenderData);
			assertTrue(latestCopyHasBatteryRenderData);
			assertEquals(cacheChanges, finalCacheChanges);
		});
	}

	@Test
	void getInventoryForInputOutputWritesDisplayItemChangesToLatestBoundStack() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			var inventoryHandler = wrapper.getInventoryForInputOutput();
			wrapper.getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class).selectSlot(0);
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			RenderData originalRenderData = stack.get(ModCoreDataComponents.RENDER_DATA).copy();
			ItemStack insertionStack = stack.copy();

			IBackpackWrapper insertionWrapper = BackpackWrapper.fromStack(insertionStack);
			int inserted;
			try (Transaction tx = Transaction.openRoot()) {
				inserted = inventoryHandler.insert(0, ItemResource.of(new ItemStack(Items.STICK)), 1, tx);
				tx.commit();
			}
			ItemStack extractionStack = insertionStack.copy();
			IBackpackWrapper extractionWrapper = BackpackWrapper.fromStack(extractionStack);
			int extracted;
			try (Transaction tx = Transaction.openRoot()) {
				extracted = inventoryHandler.extract(0, ItemResource.of(new ItemStack(Items.STICK)), 1, tx);
				tx.commit();
			}
			ItemStack originalDisplayItem = stack.get(ModCoreDataComponents.RENDER_DATA).display().displayItems().getFirst().createItemStack();
			ItemStack insertionDisplayItem = insertionStack.get(ModCoreDataComponents.RENDER_DATA).display().displayItems().getFirst().createItemStack();
			ItemStack extractionDisplayItem = extractionStack.get(ModCoreDataComponents.RENDER_DATA).display().displayItems().getFirst().createItemStack();

			assertSame(wrapper, insertionWrapper);
			assertSame(wrapper, extractionWrapper);
			assertEquals(1, inserted);
			assertEquals(1, extracted);
			assertEquals(originalRenderData, stack.get(ModCoreDataComponents.RENDER_DATA));
			assertTrue(originalDisplayItem.isEmpty());
			assertTrue(insertionDisplayItem.is(Items.STICK));
			assertTrue(extractionDisplayItem.isEmpty());
		});
	}

	@Test
	void getItemInventoryHandlerExchangesDisplayRenderDataToAccessStackOnCommit() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			wrapper.getInventoryHandler();
			wrapper.getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class).selectSlot(0);
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			ItemAccess itemAccess = ItemAccess.forStack(stack);
			var itemHandler = BackpackWrapper.getItemInventoryHandler(itemAccess);
			RenderData originalRenderData = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA).copy();

			int rolledBackInsertion;
			try (Transaction tx = Transaction.openRoot()) {
				rolledBackInsertion = itemHandler.insert(0, ItemResource.of(new ItemStack(Items.STICK)), 1, tx);
			}
			RenderData renderDataAfterRollback = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA).copy();
			long amountAfterRollback = itemHandler.getAmountAsLong(0);
			int committedInsertion;
			try (Transaction tx = Transaction.openRoot()) {
				committedInsertion = itemHandler.insert(0, ItemResource.of(new ItemStack(Items.STICK)), 1, tx);
				tx.commit();
			}
			RenderData renderDataAfterCommit = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA);
			ItemStack displayedItemAfterCommit = renderDataAfterCommit.display().displayItems().getFirst().createItemStack();

			assertEquals(1, rolledBackInsertion);
			assertEquals(originalRenderData, renderDataAfterRollback);
			assertEquals(0, amountAfterRollback);
			assertEquals(1, committedInsertion);
			assertTrue(displayedItemAfterCommit.is(Items.STICK));
		});
	}

	@Test
	void getEnergyHandlerReusesInitializedInceptionHandlerForCopiedStacks() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper parent = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			IBackpackWrapper child = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			parent.setSlotNumbers(1, 1);
			child.setSlotNumbers(1, 1);
			parent.getInventoryHandler();
			child.getInventoryHandler();
			child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			parent.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
			parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			UUID parentUuid = parent.getContentsUuid().orElseThrow();
			storageUuids.add(parentUuid);
			storageUuids.add(child.getContentsUuid().orElseThrow());
			EnergyHandler childEnergyHandler = child.getEnergyHandler().orElseThrow();
			EnergyHandler energyHandler = parent.getEnergyHandler().orElseThrow();
			boolean reusedParentWrapper = true;
			boolean reusedEnergyHandler = true;

			for (int i = 0; i < 1_000; i++) {
				IBackpackWrapper copiedWrapper = BackpackWrapper.fromStack(parent.getBackpack().copy());
				reusedParentWrapper &= parent == copiedWrapper;
				reusedEnergyHandler &= energyHandler == copiedWrapper.getEnergyHandler().orElseThrow();
			}
			long childCapacity = childEnergyHandler.getCapacityAsLong();
			long parentCapacity = energyHandler.getCapacityAsLong();

			assertEquals(childCapacity, parentCapacity);
			assertTrue(reusedParentWrapper);
			assertTrue(reusedEnergyHandler);
		});
	}

	@Test
	void getEnergyHandlerWritesBatteryRenderDataToLatestBoundStack() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			wrapper.getInventoryHandler();
			wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			EnergyHandler energyHandler = wrapper.getEnergyHandler().orElseThrow();
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			RenderData originalRenderData = stack.get(ModCoreDataComponents.RENDER_DATA).copy();
			ItemStack latestStack = stack.copy();

			IBackpackWrapper latestWrapper = BackpackWrapper.fromStack(latestStack);
			int inserted;
			try (Transaction tx = Transaction.openRoot()) {
				inserted = energyHandler.insert(100, tx);
				tx.commit();
			}
			RenderData latestRenderData = latestStack.get(ModCoreDataComponents.RENDER_DATA);

			assertSame(wrapper, latestWrapper);
			assertTrue(inserted > 0);
			assertEquals(originalRenderData, stack.get(ModCoreDataComponents.RENDER_DATA));
			assertTrue(latestRenderData.battery().orElseThrow().chargeRatio() > 0);
		});
	}

	@Test
	void getItemEnergyHandlerExchangesBatteryRenderDataToAccessStackOnCommit() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			wrapper.getInventoryHandler();
			wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			ItemAccess itemAccess = ItemAccess.forStack(stack);
			EnergyHandler energyHandler = BackpackWrapper.getItemEnergyHandler(itemAccess).orElseThrow();
			RenderData originalRenderData = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA).copy();

			int rolledBackInsertion;
			try (Transaction tx = Transaction.openRoot()) {
				rolledBackInsertion = energyHandler.insert(100, tx);
			}
			RenderData renderDataAfterRollback = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA).copy();
			long amountAfterRollback = energyHandler.getAmountAsLong();
			int committedInsertion;
			try (Transaction tx = Transaction.openRoot()) {
				committedInsertion = energyHandler.insert(100, tx);
				tx.commit();
			}
			RenderData renderDataAfterCommit = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA);

			assertTrue(rolledBackInsertion > 0);
			assertEquals(originalRenderData, renderDataAfterRollback);
			assertEquals(0, amountAfterRollback);
			assertTrue(committedInsertion > 0);
			assertTrue(renderDataAfterCommit.battery().orElseThrow().chargeRatio() > 0);
		});
	}

	@Test
	void getItemEnergyHandlerDoesNotOverwriteDifferentBackpackAtRootCommit() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper firstWrapper = createBatteryBackpack();
			IBackpackWrapper replacementWrapper = createBatteryBackpack();
			UUID replacementUuid = replacementWrapper.getContentsUuid().orElseThrow();
			ItemAccess itemAccess = ItemAccess.forStack(firstWrapper.getBackpack());
			EnergyHandler energyHandler = BackpackWrapper.getItemEnergyHandler(itemAccess).orElseThrow();

			int inserted;
			int exchanged;
			try (Transaction tx = Transaction.openRoot()) {
				inserted = energyHandler.insert(100, tx);
				exchanged = itemAccess.exchange(ItemResource.of(replacementWrapper.getBackpack()), 1, tx);
				tx.commit();
			}
			UUID sourceUuid = itemAccess.getResource().get(ModCoreDataComponents.STORAGE_UUID);
			long firstEnergy = firstWrapper.getEnergyHandler().orElseThrow().getAmountAsLong();

			assertTrue(inserted > 0);
			assertEquals(1, exchanged);
			assertEquals(replacementUuid, sourceUuid);
			assertTrue(firstEnergy > 0);
		});
	}

	@Test
	void getItemEnergyHandlerKeepsOuterPendingExchangeAfterNestedReplacementRollback() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper firstWrapper = createBatteryBackpack();
			IBackpackWrapper nestedWrapper = createBatteryBackpack();
			UUID firstUuid = firstWrapper.getContentsUuid().orElseThrow();
			ItemAccess itemAccess = ItemAccess.forStack(firstWrapper.getBackpack());
			EnergyHandler energyHandler = BackpackWrapper.getItemEnergyHandler(itemAccess).orElseThrow();

			int firstInsertion;
			int nestedExchange;
			int nestedInsertion;
			try (Transaction tx = Transaction.openRoot()) {
				firstInsertion = energyHandler.insert(100, tx);
				try (Transaction nestedTx = Transaction.open(tx)) {
					nestedExchange = itemAccess.exchange(ItemResource.of(nestedWrapper.getBackpack()), 1, nestedTx);
					nestedInsertion = energyHandler.insert(100, nestedTx);
				}
				tx.commit();
			}
			ItemStack sourceStack = itemAccess.getResource().toStack();
			long firstEnergy = firstWrapper.getEnergyHandler().orElseThrow().getAmountAsLong();
			long nestedEnergy = nestedWrapper.getEnergyHandler().orElseThrow().getAmountAsLong();

			assertTrue(firstInsertion > 0);
			assertEquals(1, nestedExchange);
			assertTrue(nestedInsertion > 0);
			assertEquals(firstUuid, sourceStack.get(ModCoreDataComponents.STORAGE_UUID));
			assertTrue(sourceStack.get(ModCoreDataComponents.RENDER_DATA).battery().orElseThrow().chargeRatio() > 0);
			assertTrue(firstEnergy > 0);
			assertEquals(0, nestedEnergy);
		});
	}

	@Test
	void getItemEnergyHandlerPreservesCurrentStackComponentsDuringDeferredProjection() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper wrapper = createBatteryBackpack();
			ItemAccess itemAccess = ItemAccess.forStack(wrapper.getBackpack());
			EnergyHandler energyHandler = BackpackWrapper.getItemEnergyHandler(itemAccess).orElseThrow();
			Component changedName = Component.literal("Changed after capability mutation");

			int inserted;
			int exchanged;
			try (Transaction tx = Transaction.openRoot()) {
				inserted = energyHandler.insert(100, tx);
				ItemStack changedStack = itemAccess.getResource().toStack();
				changedStack.set(DataComponents.CUSTOM_NAME, changedName);
				exchanged = itemAccess.exchange(ItemResource.of(changedStack), 1, tx);
				tx.commit();
			}
			ItemStack sourceStack = itemAccess.getResource().toStack();

			assertTrue(inserted > 0);
			assertEquals(1, exchanged);
			assertEquals(changedName, sourceStack.get(DataComponents.CUSTOM_NAME));
			assertTrue(sourceStack.get(ModCoreDataComponents.RENDER_DATA).battery().orElseThrow().chargeRatio() > 0);
		});
	}

	@Test
	void getItemEnergyHandlerPreservesLinkedEndpointComponentsDuringDeferredProjection() throws Throwable {
		runOnServerThread(() -> {
			UUID groupId = UUID.randomUUID();
			ContainerContents contents = new ContainerContents();
			ILinkedStorageContents linkedContents = Mockito.mock(ILinkedStorageContents.class);
			Mockito.when(linkedContents.groupId()).thenReturn(groupId);
			Mockito.when(linkedContents.contents()).thenReturn(contents);
			BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(linkedContents, new ItemStack(ModItems.BACKPACK.get()));
			host.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			LinkedStorageEndpointData endpointData = new LinkedStorageEndpointData(groupId, UUID.randomUUID());
			ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
			endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, endpointData);
			ItemAccess itemAccess = ItemAccess.forStack(endpoint);
			Component changedName = Component.literal("Changed linked endpoint");

			try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
				resolver.when(() -> BackpackLinkedStorageResolver.resolveServerCanonicalHost(Mockito.any(ItemStack.class))).thenReturn(Optional.of(host));
				EnergyHandler energyHandler = BackpackWrapper.getItemEnergyHandler(itemAccess).orElseThrow();
				int inserted;
				int exchanged;
				try (Transaction tx = Transaction.openRoot()) {
					inserted = energyHandler.insert(100, tx);
					ItemStack changedStack = itemAccess.getResource().toStack();
					changedStack.set(DataComponents.CUSTOM_NAME, changedName);
					exchanged = itemAccess.exchange(ItemResource.of(changedStack), 1, tx);
					tx.commit();
				}
				ItemStack sourceStack = itemAccess.getResource().toStack();

				assertTrue(inserted > 0);
				assertEquals(1, exchanged);
				assertEquals(endpointData, sourceStack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT));
				assertEquals(changedName, sourceStack.get(DataComponents.CUSTOM_NAME));
				assertTrue(sourceStack.get(ModCoreDataComponents.RENDER_DATA).battery().orElseThrow().chargeRatio() > 0);
			}
		});
	}

	@Test
	void getEnergyHandlerRefreshesAfterContainedUpgradeChanges() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper parent = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			IBackpackWrapper child = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			parent.setSlotNumbers(1, 1);
			child.setSlotNumbers(1, 1);
			parent.getInventoryHandler();
			child.getInventoryHandler();
			parent.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
			parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			storageUuids.add(parent.getContentsUuid().orElseThrow());
			storageUuids.add(child.getContentsUuid().orElseThrow());
			boolean parentInitiallyHasNoEnergy = parent.getEnergyHandler().isEmpty();

			child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			EnergyHandler energyHandler = parent.getEnergyHandler().orElseThrow();
			IUpgradeWrapper batteryUpgrade = child.getUpgradeHandler().getSlotWrappers().get(0);
			boolean energyHandlerHasCapacity = energyHandler.getCapacityAsLong() > 0;

			batteryUpgrade.setEnabled(false);
			boolean childHasNoEnergyWhenDisabled = child.getEnergyHandler().isEmpty();
			boolean parentHasNoEnergyWhenDisabled = parent.getEnergyHandler().isEmpty();

			batteryUpgrade.setEnabled(true);
			boolean childHasEnergyWhenEnabled = child.getEnergyHandler().isPresent();
			EnergyHandler parentHandlerWhenEnabled = parent.getEnergyHandler().orElseThrow();

			child.getUpgradeHandler().setStackInSlot(0, ItemStack.EMPTY);
			boolean parentHasNoEnergyWhenRemoved = parent.getEnergyHandler().isEmpty();

			child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			EnergyHandler parentHandlerWhenRestored = parent.getEnergyHandler().orElseThrow();

			assertTrue(parentInitiallyHasNoEnergy);
			assertTrue(energyHandlerHasCapacity);
			assertTrue(childHasNoEnergyWhenDisabled);
			assertTrue(parentHasNoEnergyWhenDisabled);
			assertTrue(childHasEnergyWhenEnabled);
			assertSame(energyHandler, parentHandlerWhenEnabled);
			assertTrue(parentHasNoEnergyWhenRemoved);
			assertSame(energyHandler, parentHandlerWhenRestored);
		});
	}

	@Test
	void getEnergyHandlerKeepsRemainingInceptionParentSubscribedToSharedChild() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper firstParent = createOneSlotInceptionBackpack();
			IBackpackWrapper secondParent = createOneSlotInceptionBackpack();
			IBackpackWrapper child = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			child.setSlotNumbers(1, 1);
			child.getInventoryHandler();
			child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			firstParent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			secondParent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			storageUuids.add(child.getContentsUuid().orElseThrow());

			boolean firstParentHasEnergy = firstParent.getEnergyHandler().isPresent();
			int callbacksAfterFirstParent = getRefreshCallbackCount(child.getUpgradeHandler());
			boolean secondParentHasEnergy = secondParent.getEnergyHandler().isPresent();
			int callbacksAfterSecondParent = getRefreshCallbackCount(child.getUpgradeHandler());
			secondParent.getInventoryHandler().setStackInSlot(0, ItemStack.EMPTY);
			int callbacksAfterSecondParentRemoved = getRefreshCallbackCount(child.getUpgradeHandler());
			child.getUpgradeHandler().getSlotWrappers().get(0).setEnabled(false);
			boolean firstParentHasNoEnergyAfterChildDisabled = firstParent.getEnergyHandler().isEmpty();

			assertTrue(firstParentHasEnergy);
			assertEquals(1, callbacksAfterFirstParent);
			assertTrue(secondParentHasEnergy);
			assertEquals(2, callbacksAfterSecondParent);
			assertEquals(1, callbacksAfterSecondParentRemoved);
			assertTrue(firstParentHasNoEnergyAfterChildDisabled);
		});
	}

	@Test
	void getEnergyHandlerRefreshesLocalGraphWhenBatteryAndInceptionAreToggled() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper parent = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			IBackpackWrapper child = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			parent.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 2);
			child.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 1);
			parent.getInventoryHandler();
			child.getInventoryHandler();
			child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			parent.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			parent.getUpgradeHandler().setStackInSlot(1, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
			parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			storageUuids.add(parent.getContentsUuid().orElseThrow());
			storageUuids.add(child.getContentsUuid().orElseThrow());
			EnergyHandler childEnergyHandler = child.getEnergyHandler().orElseThrow();
			long singleBatteryCapacity = childEnergyHandler.getCapacityAsLong();
			IUpgradeWrapper parentBattery = parent.getUpgradeHandler().getSlotWrappers().get(0);
			IUpgradeWrapper inception = parent.getUpgradeHandler().getSlotWrappers().get(1);
			long parentBatteryCapacity = ((EnergyHandler) parentBattery).getCapacityAsLong();
			EnergyHandler parentEnergyHandler = parent.getEnergyHandler().orElseThrow();
			long initialCapacity = parentEnergyHandler.getCapacityAsLong();
			int initialHandlerCount = parent.getUpgradeHandler().getWrappersThatImplement(EnergyHandler.class).size();

			parentBattery.setEnabled(false);
			long capacityWithParentBatteryDisabled = parent.getEnergyHandler().orElseThrow().getCapacityAsLong();

			parentBattery.setEnabled(true);
			long capacityWithParentBatteryEnabled = parent.getEnergyHandler().orElseThrow().getCapacityAsLong();

			inception.setEnabled(false);
			long capacityWithInceptionDisabled = parent.getEnergyHandler().orElseThrow().getCapacityAsLong();
			int handlersWithInceptionDisabled = parent.getUpgradeHandler().getWrappersThatImplement(EnergyHandler.class).size();
			parentBattery.setEnabled(false);
			boolean hasNoEnergyWithAllUpgradesDisabled = parent.getEnergyHandler().isEmpty();

			inception.setEnabled(true);
			long capacityWithOnlyInceptionEnabled = parent.getEnergyHandler().orElseThrow().getCapacityAsLong();
			int handlersWithOnlyInceptionEnabled = parent.getUpgradeHandler().getWrappersThatImplement(EnergyHandler.class).size();

			assertEquals(parentBatteryCapacity + singleBatteryCapacity, initialCapacity);
			assertEquals(2, initialHandlerCount);
			assertEquals(singleBatteryCapacity, capacityWithParentBatteryDisabled);
			assertEquals(parentBatteryCapacity + singleBatteryCapacity, capacityWithParentBatteryEnabled);
			assertEquals(parentBatteryCapacity, capacityWithInceptionDisabled);
			assertEquals(1, handlersWithInceptionDisabled);
			assertTrue(hasNoEnergyWithAllUpgradesDisabled);
			assertEquals(singleBatteryCapacity, capacityWithOnlyInceptionEnabled);
			assertEquals(1, handlersWithOnlyInceptionEnabled);
		});
	}

	@Test
	void getEnergyHandlerKeepsInceptionMembershipListenersAfterLocalUpgradeToggles() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper parent = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			IBackpackWrapper child = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			parent.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 2);
			child.setSlotNumbers(ModItems.BACKPACK.get().getNumberOfSlots(), 2);
			parent.getInventoryHandler();
			child.getInventoryHandler();
			parent.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			parent.getUpgradeHandler().setStackInSlot(1, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
			child.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
			child.getUpgradeHandler().setStackInSlot(1, new ItemStack(ModItems.TANK_UPGRADE.get()));
			parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			storageUuids.add(parent.getContentsUuid().orElseThrow());
			storageUuids.add(child.getContentsUuid().orElseThrow());

			IUpgradeWrapper parentBattery = parent.getUpgradeHandler().getSlotWrappers().get(0);
			InceptionUpgradeWrapper inception = (InceptionUpgradeWrapper) parent.getUpgradeHandler().getSlotWrappers().get(1);
			int parentInventorySize = parent.getInventoryHandler().size();
			int childInventorySize = child.getInventoryHandler().size();
			long parentBatteryCapacity = ((EnergyHandler) parentBattery).getCapacityAsLong();
			long childBatteryCapacity = child.getEnergyHandler().orElseThrow().getCapacityAsLong();
			parent.getInventoryForUpgradeProcessing();
			parent.getFluidHandler().orElseThrow();
			parent.getEnergyHandler().orElseThrow();
			SubBackpacksHandler subBackpacksHandler = getSubBackpacksHandler(inception);

			ListenerCounts initialListenerCounts = getInceptionListenerCounts(subBackpacksHandler);
			int initialRefreshCallbackCount = getRefreshCallbackCount(child.getUpgradeHandler());
			boolean listenersRemainStable = true;
			boolean refreshCallbackRemainsStable = true;
			for (int i = 0; i < 5; i++) {
				parentBattery.setEnabled(false);
				parentBattery.setEnabled(true);
				parent.getUpgradeHandler().getWrappersThatImplement(EnergyHandler.class);
				parent.getInventoryForUpgradeProcessing();
				parent.getFluidHandler().orElseThrow();
				parent.getEnergyHandler().orElseThrow();
				listenersRemainStable &= getInceptionListenerCounts(subBackpacksHandler).equals(new ListenerCounts(4, 1, 3));
				refreshCallbackRemainsStable &= getRefreshCallbackCount(child.getUpgradeHandler()) == 1;
			}

			parentBattery.setEnabled(false);
			ListenerCounts listenerCountsWithBatteryDisabled = getInceptionListenerCounts(subBackpacksHandler);
			parent.getInventoryHandler().setStackInSlot(0, ItemStack.EMPTY);
			var inventory = parent.getInventoryForUpgradeProcessing();
			IStorageFluidHandler fluidHandler = parent.getFluidHandler().orElseThrow();
			boolean parentHasNoEnergyWithoutChild = parent.getEnergyHandler().isEmpty();
			int inventorySizeWithoutChild = inventory.size();
			int fluidHandlerSizeWithoutChild = fluidHandler.size();
			ListenerCounts listenerCountsWithoutChild = getInceptionListenerCounts(subBackpacksHandler);
			int refreshCallbacksWithoutChild = getRefreshCallbackCount(child.getUpgradeHandler());

			parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			var restoredInventory = parent.getInventoryForUpgradeProcessing();
			int restoredInventorySize = inventory.size();
			IStorageFluidHandler restoredFluidHandler = parent.getFluidHandler().orElseThrow();
			int restoredFluidHandlerSize = fluidHandler.size();
			long capacityWithChildRestored = parent.getEnergyHandler().orElseThrow().getCapacityAsLong();
			int refreshCallbacksWithChildRestored = getRefreshCallbackCount(child.getUpgradeHandler());

			parentBattery.setEnabled(true);
			long capacityWithBatteryAndChildEnabled = parent.getEnergyHandler().orElseThrow().getCapacityAsLong();

			assertEquals(new ListenerCounts(4, 1, 3), initialListenerCounts);
			assertEquals(1, initialRefreshCallbackCount);
			assertTrue(listenersRemainStable);
			assertTrue(refreshCallbackRemainsStable);
			assertEquals(new ListenerCounts(0, 0, 0), listenerCountsWithBatteryDisabled);
			assertTrue(parentHasNoEnergyWithoutChild);
			assertEquals(parentInventorySize, inventorySizeWithoutChild);
			assertEquals(0, fluidHandlerSizeWithoutChild);
			assertEquals(new ListenerCounts(4, 1, 3), listenerCountsWithoutChild);
			assertEquals(0, refreshCallbacksWithoutChild);
			assertSame(inventory, restoredInventory);
			assertEquals(parentInventorySize + childInventorySize, restoredInventorySize);
			assertSame(fluidHandler, restoredFluidHandler);
			assertEquals(1, restoredFluidHandlerSize);
			assertEquals(childBatteryCapacity, capacityWithChildRestored);
			assertEquals(1, refreshCallbacksWithChildRestored);
			assertEquals(parentBatteryCapacity + childBatteryCapacity, capacityWithBatteryAndChildEnabled);
		});
	}

	@Test
	void getFluidHandlerRefreshesNestedInceptionWithoutDuplicatingTanks() throws Throwable {
		runOnServerThread(() -> {
			IBackpackWrapper parent = createOneSlotInceptionBackpack();
			IBackpackWrapper child = createOneSlotInceptionBackpack();
			IBackpackWrapper grandchild = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
			grandchild.setSlotNumbers(1, 1);
			grandchild.getInventoryHandler();
			grandchild.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
			child.getInventoryHandler().setStackInSlot(0, grandchild.getBackpack());
			parent.getInventoryHandler().setStackInSlot(0, child.getBackpack());
			storageUuids.add(grandchild.getContentsUuid().orElseThrow());
			IStorageFluidHandler fluidHandler = parent.getFluidHandler().orElseThrow();
			int initialTankCount = fluidHandler.size();

			grandchild.getUpgradeHandler().setStackInSlot(0, ItemStack.EMPTY);
			IStorageFluidHandler fluidHandlerWithoutTank = parent.getFluidHandler().orElseThrow();
			int tankCountWithoutTank = fluidHandler.size();

			grandchild.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
			IStorageFluidHandler restoredFluidHandler = parent.getFluidHandler().orElseThrow();
			int restoredTankCount = fluidHandler.size();

			assertEquals(1, initialTankCount);
			assertSame(fluidHandler, fluidHandlerWithoutTank);
			assertEquals(0, tankCountWithoutTank);
			assertSame(fluidHandler, restoredFluidHandler);
			assertEquals(1, restoredTankCount);
		});
	}

	@Test
	void getFluidHandlerWritesTankRenderDataToLatestBoundStack() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			wrapper.getInventoryHandler();
			wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
			IStorageFluidHandler fluidHandler = wrapper.getFluidHandler().orElseThrow();
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			RenderData originalRenderData = stack.get(ModCoreDataComponents.RENDER_DATA).copy();
			ItemStack latestStack = stack.copy();

			IBackpackWrapper latestWrapper = BackpackWrapper.fromStack(latestStack);
			int inserted;
			try (Transaction tx = Transaction.openRoot()) {
				inserted = fluidHandler.insert(0, FluidResource.of(Fluids.WATER), 1_000, tx);
				tx.commit();
			}
			RenderData latestRenderData = latestStack.get(ModCoreDataComponents.RENDER_DATA);

			assertSame(wrapper, latestWrapper);
			assertTrue(inserted > 0);
			assertEquals(originalRenderData, stack.get(ModCoreDataComponents.RENDER_DATA));
			assertTrue(latestRenderData.tanks().values().stream().anyMatch(tank -> tank.fillRatio() > 0));
		});
	}

	@Test
	void getItemFluidHandlerExchangesTankRenderDataToAccessStackTransactionally() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			wrapper.getInventoryHandler();
			wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.TANK_UPGRADE.get()));
			storageUuids.add(wrapper.getContentsUuid().orElseThrow());
			ItemAccess itemAccess = ItemAccess.forStack(stack);
			var fluidHandler = wrapper.getItemFluidHandler(itemAccess).orElseThrow();
			RenderData originalRenderData = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA).copy();

			int rolledBackInsertion;
			try (Transaction tx = Transaction.openRoot()) {
				rolledBackInsertion = fluidHandler.insert(0, FluidResource.of(Fluids.WATER), 1_000, tx);
			}
			RenderData renderDataAfterRollback = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA).copy();
			long amountAfterRollback = fluidHandler.getAmountAsLong(0);
			int committedInsertion;
			try (Transaction tx = Transaction.openRoot()) {
				committedInsertion = fluidHandler.insert(0, FluidResource.of(Fluids.WATER), 1_000, tx);
				tx.commit();
			}
			RenderData renderDataAfterCommit = itemAccess.getResource().toStack().get(ModCoreDataComponents.RENDER_DATA);

			assertTrue(rolledBackInsertion > 0);
			assertEquals(originalRenderData, renderDataAfterRollback);
			assertEquals(0, amountAfterRollback);
			assertTrue(committedInsertion > 0);
			assertTrue(renderDataAfterCommit.tanks().values().stream().anyMatch(tank -> tank.fillRatio() > 0));
		});
	}

	@Test
	void fromStackKeepsLinkedCanonicalHostForCopiedEndpointStacks() throws Throwable {
		runOnServerThread(() -> {
			UUID groupId = UUID.randomUUID();
			ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
			endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
			ItemStack firstEndpointCopy = endpoint.copy();
			ItemStack secondEndpointCopy = endpoint.copy();
			ItemStack virtualCarrier = new ItemStack(ModItems.BACKPACK.get());
			BackpackLinkedStorageHostWrapper host = Mockito.mock(BackpackLinkedStorageHostWrapper.class);
			Mockito.when(host.getBackpack()).thenReturn(virtualCarrier);

			try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
				resolver.when(() -> BackpackLinkedStorageResolver.resolveServerCanonicalHost(firstEndpointCopy)).thenReturn(Optional.of(host));
				resolver.when(() -> BackpackLinkedStorageResolver.resolveServerCanonicalHost(secondEndpointCopy)).thenReturn(Optional.of(host));

				IBackpackWrapper firstResolved = BackpackWrapper.fromStack(firstEndpointCopy);
				IBackpackWrapper secondResolved = BackpackWrapper.fromStack(secondEndpointCopy);
				ItemStack ordinaryStack = new ItemStack(ModItems.BACKPACK.get());
				ordinaryStack.set(ModCoreDataComponents.STORAGE_UUID, groupId);
				IBackpackWrapper ordinaryWrapper = BackpackWrapper.fromStack(ordinaryStack);
				boolean firstCopyHasNoOrdinaryWrapper = BackpackWrapper.fromExistingData(firstEndpointCopy).isEmpty();
				boolean secondCopyHasNoOrdinaryWrapper = BackpackWrapper.fromExistingData(secondEndpointCopy).isEmpty();

				assertNotSame(firstEndpointCopy, secondEndpointCopy);
				assertSame(host, firstResolved);
				assertSame(host, secondResolved);
				assertSame(virtualCarrier, firstResolved.getBackpack());
				assertSame(virtualCarrier, secondResolved.getBackpack());
				assertNotSame(host, ordinaryWrapper);
				assertTrue(firstCopyHasNoOrdinaryWrapper);
				assertTrue(secondCopyHasNoOrdinaryWrapper);
				Mockito.verify(host, Mockito.never()).setBackpackStack(Mockito.any());
			}
		});
	}

	@Test
	void onVirtualCarrierChangedRebindsInceptionParentAfterLinkedCanonicalTierReplacement() throws Throwable {
		runOnServerThread(() -> {
			UUID groupId = UUID.randomUUID();
			ContainerContents contents = new ContainerContents();
			ILinkedStorageContents linkedContents = Mockito.mock(ILinkedStorageContents.class);
			Mockito.when(linkedContents.groupId()).thenReturn(groupId);
			Mockito.when(linkedContents.contents()).thenReturn(contents);
			BackpackLinkedStorageHostWrapper host = new BackpackLinkedStorageHostWrapper(linkedContents, new ItemStack(ModItems.BACKPACK.get()));
			ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
			endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(groupId, UUID.randomUUID()));
			IBackpackWrapper parent = createOneSlotInceptionBackpack();

			try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
				resolver.when(() -> BackpackLinkedStorageResolver.resolveServerCanonicalHost(endpoint)).thenReturn(Optional.of(host));
				parent.getInventoryHandler().setStackInSlot(0, endpoint);
				var previousInventory = parent.getInventoryForUpgradeProcessing();
				UpgradeHandler previousHostUpgradeHandler = host.getUpgradeHandler();
				int previousInventorySize = previousInventory.size();
				int expectedPreviousInventorySize = parent.getInventoryHandler().size() + host.getInventoryHandler().size();
				int previousRefreshCallbackCount = getRefreshCallbackCount(previousHostUpgradeHandler);

				ItemStack upgradedCarrier = new ItemStack(ModItems.GOLD_BACKPACK.get());
				upgradedCarrier.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, ModItems.GOLD_BACKPACK.get().getNumberOfSlots());
				upgradedCarrier.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, ModItems.GOLD_BACKPACK.get().getNumberOfUpgradeSlots());
				CompoundTag upgradedCarrierTag = (CompoundTag) ItemStack.CODEC.encodeStart(NbtOps.INSTANCE, upgradedCarrier).getOrThrow();
				host.onVirtualCarrierChanged(upgradedCarrierTag);

				var upgradedInventory = parent.getInventoryForUpgradeProcessing();
				UpgradeHandler upgradedHostUpgradeHandler = host.getUpgradeHandler();
				int upgradedInventorySize = upgradedInventory.size();
				int expectedUpgradedInventorySize = parent.getInventoryHandler().size() + host.getInventoryHandler().size();
				int upgradedRefreshCallbackCount = getRefreshCallbackCount(upgradedHostUpgradeHandler);

				assertEquals(expectedPreviousInventorySize, previousInventorySize);
				assertEquals(1, previousRefreshCallbackCount);
				assertSame(previousInventory, upgradedInventory);
				assertEquals(expectedUpgradedInventorySize, upgradedInventorySize);
				assertNotSame(previousHostUpgradeHandler, upgradedHostUpgradeHandler);
				assertEquals(1, upgradedRefreshCallbackCount);
			}
		});
	}

	@Test
	void dedupeBackpackWrappersRemovesUuidFromDuplicatePhysicalWrapper() throws Throwable {
		runOnServerThread(() -> {
			ItemStack firstStack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper canonicalWrapper = BackpackWrapper.fromStack(firstStack);
			canonicalWrapper.getInventoryHandler();
			UUID storageUuid = canonicalWrapper.getContentsUuid().orElseThrow();
			storageUuids.add(storageUuid);
			ItemStack duplicateStack = firstStack.copy();
			IBackpackWrapper firstPhysicalWrapper = BackpackWrapper.fromStackNoUuidCache(firstStack);
			IBackpackWrapper duplicatePhysicalWrapper = BackpackWrapper.fromStackNoUuidCache(duplicateStack);

			UUIDDeduplicator.dedupeBackpackWrappers(firstPhysicalWrapper, duplicatePhysicalWrapper);
			boolean firstStackHasUuid = firstStack.has(ModCoreDataComponents.STORAGE_UUID);
			boolean duplicateStackHasNoUuid = !duplicateStack.has(ModCoreDataComponents.STORAGE_UUID);
			IBackpackWrapper resolvedFirstWrapper = BackpackWrapper.fromStack(firstStack);

			assertNotSame(firstPhysicalWrapper, duplicatePhysicalWrapper);
			assertTrue(firstStackHasUuid);
			assertTrue(duplicateStackHasNoUuid);
			assertSame(canonicalWrapper, resolvedFirstWrapper);
		});
	}

	@Test
	void checkForDuplicateBackpacksAndRemoveTheirUUIDKeepsLargestBackpack() throws Throwable {
		runOnServerThread(() -> {
			ItemStack firstStack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper canonicalWrapper = BackpackWrapper.fromStack(firstStack);
			canonicalWrapper.getInventoryHandler();
			UUID storageUuid = canonicalWrapper.getContentsUuid().orElseThrow();
			storageUuids.add(storageUuid);
			ItemStack middleStack = firstStack.copy();
			middleStack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 36);
			ItemStack largestStack = firstStack.copy();
			largestStack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 45);
			Player player = Mockito.mock(Player.class);
			PlayerInventoryProvider inventoryProvider = Mockito.mock(PlayerInventoryProvider.class);

			try (MockedStatic<PlayerInventoryProvider> provider = Mockito.mockStatic(PlayerInventoryProvider.class)) {
				provider.when(PlayerInventoryProvider::get).thenReturn(inventoryProvider);
				Mockito.doAnswer(invocation -> {
					PlayerInventoryProvider.BackpackInventorySlotConsumer consumer = invocation.getArgument(1);
					consumer.accept(firstStack, PlayerInventoryProvider.MAIN_INVENTORY, "", 0);
					consumer.accept(middleStack, PlayerInventoryProvider.MAIN_INVENTORY, "", 1);
					consumer.accept(largestStack, PlayerInventoryProvider.MAIN_INVENTORY, "", 2);
					return false;
				}).when(inventoryProvider).runOnBackpacks(Mockito.eq(player), Mockito.any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));

				UUIDDeduplicator.checkForDuplicateBackpacksAndRemoveTheirUUID(player, storageUuid, firstStack);
			}
			boolean firstStackHasNoUuid = !firstStack.has(ModCoreDataComponents.STORAGE_UUID);
			boolean middleStackHasNoUuid = !middleStack.has(ModCoreDataComponents.STORAGE_UUID);
			boolean largestStackHasUuid = largestStack.has(ModCoreDataComponents.STORAGE_UUID);
			ItemStack resolvedLargestStack = BackpackWrapper.fromStack(largestStack).getBackpack();

			assertTrue(firstStackHasNoUuid);
			assertTrue(middleStackHasNoUuid);
			assertTrue(largestStackHasUuid);
			assertSame(largestStack, resolvedLargestStack);
		});
	}

	@Test
	void checkForDuplicateBackpacksAndRemoveTheirUUIDIgnoresMatchingLinkedEndpoint() throws Throwable {
		runOnServerThread(() -> {
			ItemStack ordinaryStack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper ordinaryWrapper = BackpackWrapper.fromStack(ordinaryStack);
			ordinaryWrapper.getInventoryHandler();
			UUID storageUuid = ordinaryWrapper.getContentsUuid().orElseThrow();
			storageUuids.add(storageUuid);
			ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
			endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(storageUuid, UUID.randomUUID()));
			Player player = Mockito.mock(Player.class);
			PlayerInventoryProvider inventoryProvider = Mockito.mock(PlayerInventoryProvider.class);

			try (MockedStatic<PlayerInventoryProvider> provider = Mockito.mockStatic(PlayerInventoryProvider.class)) {
				provider.when(PlayerInventoryProvider::get).thenReturn(inventoryProvider);
				Mockito.doAnswer(invocation -> {
					PlayerInventoryProvider.BackpackInventorySlotConsumer consumer = invocation.getArgument(1);
					consumer.accept(ordinaryStack, PlayerInventoryProvider.MAIN_INVENTORY, "", 0);
					consumer.accept(endpoint, PlayerInventoryProvider.MAIN_INVENTORY, "", 1);
					return false;
				}).when(inventoryProvider).runOnBackpacks(Mockito.eq(player), Mockito.any(PlayerInventoryProvider.BackpackInventorySlotConsumer.class));

				UUIDDeduplicator.checkForDuplicateBackpacksAndRemoveTheirUUID(player, storageUuid, ordinaryStack);
			}
			boolean ordinaryStackHasUuid = ordinaryStack.has(ModCoreDataComponents.STORAGE_UUID);
			boolean endpointHasLinkedStorage = endpoint.has(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);

			assertTrue(ordinaryStackHasUuid);
			assertTrue(endpointHasLinkedStorage);
		});
	}

	@Test
	void dedupeBackpackItemEntityInAreaIgnoresMatchingLinkedEndpoint() throws Throwable {
		runOnServerThread(() -> {
			ItemStack ordinaryStack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper ordinaryWrapper = BackpackWrapper.fromStack(ordinaryStack);
			ordinaryWrapper.getInventoryHandler();
			UUID storageUuid = ordinaryWrapper.getContentsUuid().orElseThrow();
			storageUuids.add(storageUuid);
			ItemStack endpoint = new ItemStack(ModItems.BACKPACK.get());
			endpoint.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(storageUuid, UUID.randomUUID()));
			ItemEntity ordinaryEntity = Mockito.mock(ItemEntity.class);
			ItemEntity endpointEntity = Mockito.mock(ItemEntity.class);
			Level level = Mockito.mock(Level.class);
			AABB searchBounds = new AABB(0, 0, 0, 1, 1, 1);
			Mockito.when(ordinaryEntity.getItem()).thenReturn(ordinaryStack);
			Mockito.when(ordinaryEntity.level()).thenReturn(level);
			Mockito.when(ordinaryEntity.getBoundingBox()).thenReturn(searchBounds);
			Mockito.when(endpointEntity.getItem()).thenReturn(endpoint);
			Mockito.when(level.getEntitiesOfClass(Mockito.eq(ItemEntity.class), Mockito.any(AABB.class),
					Mockito.<java.util.function.Predicate<? super ItemEntity>>any())).thenReturn(List.of(endpointEntity));

			UUIDDeduplicator.dedupeBackpackItemEntityInArea(ordinaryEntity);
			boolean ordinaryStackHasUuid = ordinaryStack.has(ModCoreDataComponents.STORAGE_UUID);
			boolean endpointHasLinkedStorage = endpoint.has(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);

			assertTrue(ordinaryStackHasUuid);
			assertTrue(endpointHasLinkedStorage);
		});
	}

	@Test
	void dedupeBackpackWrappersKeepsOrdinaryWrapperWhenLinkedWrapperLeaksThrough() {
		IBackpackWrapper ordinaryWrapper = Mockito.mock(IBackpackWrapper.class);
		BackpackLinkedStorageHostWrapper linkedWrapper = Mockito.mock(BackpackLinkedStorageHostWrapper.class);

		IBackpackWrapper linkedFirstResult = UUIDDeduplicator.dedupeBackpackWrappers(linkedWrapper, ordinaryWrapper);
		IBackpackWrapper ordinaryFirstResult = UUIDDeduplicator.dedupeBackpackWrappers(ordinaryWrapper, linkedWrapper);

		assertSame(ordinaryWrapper, linkedFirstResult);
		assertSame(ordinaryWrapper, ordinaryFirstResult);
		Mockito.verify(ordinaryWrapper, Mockito.never()).removeContentsUUIDTag();
		Mockito.verify(linkedWrapper, Mockito.never()).removeContentsUUIDTag();
	}

	@Test
	void setSlotNumbersRebuildsCanonicalSizeDependentHandlers() throws Throwable {
		runOnServerThread(() -> {
			ItemStack source = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(source);
			InventoryHandler originalInventoryHandler = wrapper.getInventoryHandler();
			UpgradeHandler originalUpgradeHandler = wrapper.getUpgradeHandler();
			UUID storageUuid = wrapper.getContentsUuid().orElseThrow();
			storageUuids.add(storageUuid);
			ItemStack upgraded = new ItemStack(ModItems.GOLD_BACKPACK.get());
			upgraded.applyComponents(source.getComponentsPatch());
			BackpackItem upgradedItem = ModItems.GOLD_BACKPACK.get();

			IBackpackWrapper upgradedWrapper = BackpackWrapper.fromStack(upgraded);
			upgradedWrapper.setSlotNumbers(upgradedItem.getNumberOfSlots(), upgradedItem.getNumberOfUpgradeSlots());
			InventoryHandler upgradedInventoryHandler = upgradedWrapper.getInventoryHandler();
			UpgradeHandler upgradedUpgradeHandler = upgradedWrapper.getUpgradeHandler();
			int upgradedInventorySize = upgradedInventoryHandler.size();
			int upgradedUpgradeSize = upgradedUpgradeHandler.size();

			assertSame(wrapper, upgradedWrapper);
			assertNotSame(originalInventoryHandler, upgradedInventoryHandler);
			assertNotSame(originalUpgradeHandler, upgradedUpgradeHandler);
			assertEquals(upgradedItem.getNumberOfSlots(), upgradedInventorySize);
			assertEquals(upgradedItem.getNumberOfUpgradeSlots(), upgradedUpgradeSize);
		});
	}

	@Test
	void setContentsUuidEvictsOldMappingAndRefreshesHandlers() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			InventoryHandler originalInventoryHandler = wrapper.getInventoryHandler();
			UpgradeHandler originalUpgradeHandler = wrapper.getUpgradeHandler();
			UUID originalUuid = wrapper.getContentsUuid().orElseThrow();
			ItemStack staleCopy = stack.copy();
			UUID reassignedUuid = UUID.randomUUID();
			storageUuids.add(originalUuid);
			storageUuids.add(reassignedUuid);

			wrapper.setContentsUuid(reassignedUuid);
			UUID currentUuid = wrapper.getContentsUuid().orElseThrow();
			InventoryHandler reassignedInventoryHandler = wrapper.getInventoryHandler();
			UpgradeHandler reassignedUpgradeHandler = wrapper.getUpgradeHandler();
			IBackpackWrapper staleCopyWrapper = BackpackWrapper.fromStack(staleCopy);

			assertEquals(reassignedUuid, currentUuid);
			assertNotSame(originalInventoryHandler, reassignedInventoryHandler);
			assertNotSame(originalUpgradeHandler, reassignedUpgradeHandler);
			assertNotSame(wrapper, staleCopyWrapper);
		});
	}

	@Test
	void matchesRejectsSecondaryAndAcceptsPrimaryLinkedEndpoints() throws Throwable {
		runOnServerThread(() -> {
			SmithingBackpackUpgradeRecipe recipe = new SmithingBackpackUpgradeRecipe(Optional.empty(), Ingredient.of(ModItems.BACKPACK.get()), Optional.empty(),
					new ItemStack(ModItems.GOLD_BACKPACK.get()));
			ItemStack source = new ItemStack(ModItems.BACKPACK.get());
			source.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
			source.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, false);
			SmithingRecipeInput input = new SmithingRecipeInput(ItemStack.EMPTY, source, ItemStack.EMPTY);
			boolean matchesSecondaryEndpoint = recipe.matches(input, null);
			source.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
			boolean matchesPrimaryEndpoint = recipe.matches(input, null);

			assertFalse(matchesSecondaryEndpoint);
			assertTrue(matchesPrimaryEndpoint);
		});
	}

	@Test
	void assembleResizesOnlyLinkedPrimaryPhysicalResult() throws Throwable {
		runOnServerThread(() -> {
			SmithingBackpackUpgradeRecipe recipe = new SmithingBackpackUpgradeRecipe(Optional.empty(), Ingredient.of(ModItems.BACKPACK.get()), Optional.empty(),
					new ItemStack(ModItems.GOLD_BACKPACK.get()));
			ItemStack source = new ItemStack(ModItems.BACKPACK.get());
			source.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
			source.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
			SmithingRecipeInput input = new SmithingRecipeInput(ItemStack.EMPTY, source, ItemStack.EMPTY);

			try (MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class)) {
				ItemStack result = recipe.assemble(input);
				boolean resultIsGoldBackpack = result.is(ModItems.GOLD_BACKPACK.get());
				boolean resultHasLinkedStorage = result.has(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
				int resultInventorySlots = result.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS);
				int resultUpgradeSlots = result.get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS);
				boolean sourceHasNoInventorySlots = !source.has(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS);

				assertTrue(resultIsGoldBackpack);
				assertTrue(resultHasLinkedStorage);
				assertEquals(ModItems.GOLD_BACKPACK.get().getNumberOfSlots(), resultInventorySlots);
				assertEquals(ModItems.GOLD_BACKPACK.get().getNumberOfUpgradeSlots(), resultUpgradeSlots);
				assertTrue(sourceHasNoInventorySlots);
				resolver.verifyNoInteractions();
			}
		});
	}

	@Test
	void onCraftedByCompletesLinkedPrimaryTierUpgrade() {
		ItemStack result = new ItemStack(ModItems.GOLD_BACKPACK.get());
		ServerLevel level = Mockito.mock(ServerLevel.class);
		Player player = Mockito.mock(Player.class);
		Mockito.when(player.level()).thenReturn(level);

		try (MockedStatic<BackpackLinkedStorageEndpointAdapter> adapter = Mockito.mockStatic(BackpackLinkedStorageEndpointAdapter.class)) {
			ModItems.GOLD_BACKPACK.get().onCraftedBy(result, player);

			adapter.verify(() -> BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(level, result));
		}
	}

	@Test
	void removeContentsUuidEvictsCanonicalWrapperAndRefreshesHandlers() throws Throwable {
		runOnServerThread(() -> {
			ItemStack stack = new ItemStack(ModItems.BACKPACK.get());
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(stack);
			InventoryHandler inventoryHandler = wrapper.getInventoryHandler();
			UUID removedUuid = wrapper.getContentsUuid().orElseThrow();
			storageUuids.add(removedUuid);
			ItemStack staleCopy = stack.copy();

			wrapper.removeContentsUuid();
			boolean stackHasNoUuid = !stack.has(ModCoreDataComponents.STORAGE_UUID);
			boolean removedContentsAreAbsent = BackpackStorage.get().getBackpackContents(removedUuid).isEmpty();
			IBackpackWrapper staleCopyWrapper = BackpackWrapper.fromStack(staleCopy);
			InventoryHandler refreshedInventoryHandler = wrapper.getInventoryHandler();
			UUID refreshedUuid = wrapper.getContentsUuid().orElseThrow();
			storageUuids.add(refreshedUuid);

			assertTrue(stackHasNoUuid);
			assertTrue(removedContentsAreAbsent);
			assertNotSame(wrapper, staleCopyWrapper);
			assertNotSame(inventoryHandler, refreshedInventoryHandler);
		});
	}

	private IBackpackWrapper createOneSlotInceptionBackpack() {
		IBackpackWrapper wrapper = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
		wrapper.setSlotNumbers(1, 1);
		wrapper.getInventoryHandler();
		wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.INCEPTION_UPGRADE.get()));
		storageUuids.add(wrapper.getContentsUuid().orElseThrow());
		return wrapper;
	}

	private IBackpackWrapper createBatteryBackpack() {
		IBackpackWrapper wrapper = BackpackWrapper.fromStack(new ItemStack(ModItems.BACKPACK.get()));
		wrapper.getInventoryHandler();
		wrapper.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.BATTERY_UPGRADE.get()));
		storageUuids.add(wrapper.getContentsUuid().orElseThrow());
		return wrapper;
	}

	private static int getRefreshCallbackCount(UpgradeHandler upgradeHandler) throws ReflectiveOperationException {
		java.lang.reflect.Field refreshCallbacksField = UpgradeHandler.class.getDeclaredField("refreshCallbacks");
		refreshCallbacksField.setAccessible(true);
		return ((Set<?>) refreshCallbacksField.get(upgradeHandler)).size();
	}

	private static SubBackpacksHandler getSubBackpacksHandler(InceptionUpgradeWrapper inceptionUpgradeWrapper) throws ReflectiveOperationException {
		java.lang.reflect.Field subBackpacksHandlerField = InceptionUpgradeWrapper.class.getDeclaredField("subBackpacksHandler");
		subBackpacksHandlerField.setAccessible(true);
		return (SubBackpacksHandler) subBackpacksHandlerField.get(inceptionUpgradeWrapper);
	}

	private static ListenerCounts getInceptionListenerCounts(SubBackpacksHandler subBackpacksHandler) throws ReflectiveOperationException {
		return new ListenerCounts(getListenerCount(subBackpacksHandler, "refreshListeners"), getListenerCount(subBackpacksHandler, "beforeRefreshListeners"),
				getListenerCount(subBackpacksHandler, "upgradeCachesInvalidatedListeners"));
	}

	private record ListenerCounts(int refreshListeners, int beforeRefreshListeners, int upgradeCachesInvalidatedListeners) {
	}

	private static int getListenerCount(SubBackpacksHandler subBackpacksHandler, String fieldName) throws ReflectiveOperationException {
		java.lang.reflect.Field listenerField = SubBackpacksHandler.class.getDeclaredField(fieldName);
		listenerField.setAccessible(true);
		return ((Set<?>) listenerField.get(subBackpacksHandler)).size();
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
		}, "backpack-wrapper-uuid-cache-test");
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
