package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraftforge.energy.IEnergyStorage;
import net.minecraftforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackFluidHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeWrapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.IntConsumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.*;

class InceptionGraphLifecycleTest {
	@BeforeAll
	static void bootstrapRegistries() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@Test
	void ensureInventoryListenerRegisteredReconcilesChangesWithoutAccumulatingCallbacks() throws ReflectiveOperationException {
		InventoryHandler inventory = mock(InventoryHandler.class);
		when(inventory.getSlots()).thenReturn(0);
		SubBackpacksHandler children = new SubBackpacksHandler(inventory);
		AtomicInteger notifications = new AtomicInteger();
		children.addRefreshListener(ignored -> notifications.incrementAndGet());
		Field field = SubBackpacksHandler.class.getDeclaredField("subBackpacks");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		Map<Integer, IStorageWrapper> tracked = (Map<Integer, IStorageWrapper>) field.get(children);
		tracked.put(0, mock(IBackpackWrapper.class));

		children.ensureInventoryListenerRegistered();
		children.ensureInventoryListenerRegistered();

		assertEquals(0, children.getSubBackpacks().size());
		assertEquals(1, notifications.get());
		ArgumentCaptor<IntConsumer> listener = ArgumentCaptor.forClass(IntConsumer.class);
		verify(inventory, times(3)).addListener(listener.capture());
		assertSame(listener.getAllValues().get(0), listener.getAllValues().get(2));
		verify(inventory, times(3)).removeListener(listener.getValue());
	}
	@Test
	void sharedChildUpgradeRefreshNotifiesBothParentsAndDetachesOnlyClosedParent() throws ReflectiveOperationException {
		InventoryHandler inventory = mock(InventoryHandler.class);
		IStorageWrapper child = mock(IStorageWrapper.class);
		UpgradeHandler childUpgrades = mock(UpgradeHandler.class);
		when(child.getUpgradeHandler()).thenReturn(childUpgrades);
		SubBackpacksHandler firstChildren = children(inventory, child);
		SubBackpacksHandler secondChildren = children(inventory, child);
		IStorageWrapper firstParent = mock(IStorageWrapper.class);
		IStorageWrapper secondParent = mock(IStorageWrapper.class);
		UpgradeHandler firstUpgrades = mock(UpgradeHandler.class);
		UpgradeHandler secondUpgrades = mock(UpgradeHandler.class);
		when(firstParent.getUpgradeHandler()).thenReturn(firstUpgrades);
		when(secondParent.getUpgradeHandler()).thenReturn(secondUpgrades);
		InceptionWrapperAccessor first = new InceptionWrapperAccessor(firstParent, firstChildren);
		InceptionWrapperAccessor second = new InceptionWrapperAccessor(secondParent, secondChildren);
		ArgumentCaptor<Runnable> callbacks = ArgumentCaptor.forClass(Runnable.class);
		verify(childUpgrades, times(2)).addRefreshCallback(callbacks.capture());

		callbacks.getAllValues().forEach(Runnable::run);
		verify(firstUpgrades).notifyRefreshCallbacks();
		verify(secondUpgrades).notifyRefreshCallbacks();
		first.onBeforeDeconstruct();
		verify(childUpgrades).removeRefreshCallback(callbacks.getAllValues().get(0));
		callbacks.getAllValues().get(1).run();
		verify(secondUpgrades, times(2)).notifyRefreshCallbacks();
		second.onBeforeDeconstruct();
	}

	@Test
	void retainedInventoryRefreshesLazilyAfterChildUpgradeInvalidationAndDetachesBothListeners() throws ReflectiveOperationException {
		IStorageWrapper child = mock(IStorageWrapper.class);
		ITrackedContentsItemHandler initial = mock(ITrackedContentsItemHandler.class);
		ITrackedContentsItemHandler replacement = mock(ITrackedContentsItemHandler.class);
		when(initial.getSlots()).thenReturn(1);
		when(replacement.getSlots()).thenReturn(2);
		ItemStack childItem = new ItemStack(Items.DIAMOND);
		when(replacement.getStackInSlot(1)).thenReturn(childItem);
		when(child.getInventoryForInputOutput()).thenReturn(initial, replacement);
		SubBackpacksHandler children = children(mock(InventoryHandler.class), child);
		ITrackedContentsItemHandler base = mock(ITrackedContentsItemHandler.class);
		when(base.getSlots()).thenReturn(1);
		InceptionInventoryHandler retained = new InceptionInventoryHandler(base, InventoryOrder.MAIN_FIRST, children);

		assertEquals(2, retained.getSlots());
		children.onUpgradeCachesInvalidated();
		verify(child).getInventoryForInputOutput();
		assertEquals(3, retained.getSlots());
		assertSame(childItem, retained.getStackInSlot(2));
		verify(child, times(2)).getInventoryForInputOutput();
		retained.onBeforeDeconstruct();
		children.onUpgradeCachesInvalidated();
		assertEquals(3, retained.getSlots());
		verify(child, times(2)).getInventoryForInputOutput();
	}

	@Test
	void energyStorageUsesChildMainStorageOnlyAndRefreshesAfterInvalidation() throws ReflectiveOperationException {
		IStorageWrapper child = mock(IStorageWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		IEnergyStorage local = mock(IEnergyStorage.class);
		when(local.getEnergyStored()).thenReturn(8, 13);
		when(child.getUpgradeHandler()).thenReturn(upgrades);
		when(child.getEnergyStorage()).thenReturn(Optional.of(local), Optional.empty(), Optional.of(local));
		SubBackpacksHandler children = children(mock(InventoryHandler.class), child);
		InceptionEnergyStorage energy = new InceptionEnergyStorage(null, InventoryOrder.MAIN_FIRST, children);

		assertEquals(8, energy.getEnergyStored());
		children.onUpgradeCachesInvalidated();
		verify(child).getEnergyStorage();
		assertEquals(0, energy.getEnergyStored());
		children.onUpgradeCachesInvalidated();
		verify(child, times(2)).getEnergyStorage();
		assertEquals(13, energy.getEnergyStored());
		energy.onBeforeDeconstruct();
		children.onUpgradeCachesInvalidated();
		assertEquals(13, energy.getEnergyStored());
		verify(child, times(3)).getEnergyStorage();
	}

	@Test
	void nestedFluidGraphCountsGrandchildTankOnceAndRefreshesRetainedParentAfterTankToggles() throws ReflectiveOperationException {
		IBackpackWrapper grandchild = mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		TankUpgradeWrapper tank = mock(TankUpgradeWrapper.class);
		when(grandchild.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getTypeWrappers(TankUpgradeItem.TYPE)).thenReturn(List.of(tank), List.of(), List.of(tank));
		when(grandchild.getFluidHandler()).thenAnswer(ignored -> Optional.of(new BackpackFluidHandler(grandchild)));
		SubBackpacksHandler grandchildren = children(mock(InventoryHandler.class), grandchild);
		IStorageFluidHandler childFluid = new InceptionFluidHandler(null, ItemStack.EMPTY, InventoryOrder.MAIN_FIRST, grandchildren);
		IBackpackWrapper child = mock(IBackpackWrapper.class);
		when(child.getFluidHandler()).thenReturn(Optional.of(childFluid));
		SubBackpacksHandler children = children(mock(InventoryHandler.class), child);
		InceptionFluidHandler parentFluid = new InceptionFluidHandler(null, ItemStack.EMPTY, InventoryOrder.MAIN_FIRST, children);

		assertEquals(1, parentFluid.getTanks());
		grandchildren.onUpgradeCachesInvalidated();
		children.onUpgradeCachesInvalidated();
		verify(child).getFluidHandler();
		assertEquals(0, parentFluid.getTanks());
		grandchildren.onUpgradeCachesInvalidated();
		children.onUpgradeCachesInvalidated();
		verify(child, times(2)).getFluidHandler();
		assertEquals(1, parentFluid.getTanks());
		parentFluid.onBeforeDeconstruct();
		children.onUpgradeCachesInvalidated();
		assertEquals(1, parentFluid.getTanks());
		verify(child, times(3)).getFluidHandler();
		((InceptionHandler) childFluid).onBeforeDeconstruct();
	}

	@Test
	void retainedFluidOperationsRefreshOnlyWhenFirstUsedAfterInvalidation() throws ReflectiveOperationException {
		IStorageWrapper child = mock(IStorageWrapper.class);
		IStorageFluidHandler original = mock(IStorageFluidHandler.class);
		IStorageFluidHandler replacement = mock(IStorageFluidHandler.class);
		when(original.getTanks()).thenReturn(1);
		when(replacement.getTanks()).thenReturn(2);
		when(child.getFluidHandler()).thenReturn(Optional.of(original), Optional.of(replacement));
		SubBackpacksHandler children = children(mock(InventoryHandler.class), child);
		InceptionFluidHandler retained = new InceptionFluidHandler(null, ItemStack.EMPTY, InventoryOrder.MAIN_FIRST, children);
		children.onUpgradeCachesInvalidated();
		verify(child).getFluidHandler();
		retained.fill(FluidStack.EMPTY, IStorageFluidHandler.FluidAction.SIMULATE);
		verify(child, times(2)).getFluidHandler();
		assertEquals(2, retained.getTanks());
		retained.onBeforeDeconstruct();
	}

	@SuppressWarnings("unchecked")
	private static SubBackpacksHandler children(InventoryHandler inventory, IStorageWrapper child) throws ReflectiveOperationException {
		when(inventory.getSlots()).thenReturn(0);
		SubBackpacksHandler children = new SubBackpacksHandler(inventory);
		Field field = SubBackpacksHandler.class.getDeclaredField("subBackpacks");
		field.setAccessible(true);
		((Map<Integer, IStorageWrapper>) field.get(children)).put(0, child);
		return children;
	}
}
