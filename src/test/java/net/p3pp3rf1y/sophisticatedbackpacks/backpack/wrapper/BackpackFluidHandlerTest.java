package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.world.level.material.Fluids;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.ResourceHandlerUtil;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeWrapper;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class BackpackFluidHandlerTest {
	private static final FluidResource WATER = FluidResource.of(Fluids.WATER);

	@Test
	void insertAlwaysVoidsMatchingFluidBeforeFillingTanks() {
		TankUpgradeWrapper tank = mock(TankUpgradeWrapper.class);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.ALWAYS);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(tank), List.of(voidUpgrade));

		int inserted = insert(fluidHandler);

		assertEquals(1_000, inserted);
		verify(tank, never()).insert(any(), anyInt(), any(), anyBoolean());
	}

	@Test
	void insertSlotOverflowFillsOnlyOneTankAndVoidsRemainder() {
		TankUpgradeWrapper firstTank = mock(TankUpgradeWrapper.class);
		TankUpgradeWrapper secondTank = mock(TankUpgradeWrapper.class);
		when(firstTank.getContents()).thenReturn(FluidStack.EMPTY);
		when(secondTank.getContents()).thenReturn(FluidStack.EMPTY);
		when(firstTank.insert(eq(WATER), eq(1_000), any(), eq(false))).thenReturn(500);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.SLOT_OVERFLOW);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(firstTank, secondTank), List.of(voidUpgrade));

		int inserted = insert(fluidHandler);

		assertEquals(1_000, inserted);
		verify(firstTank).insert(eq(WATER), eq(1_000), any(), eq(false));
		verify(secondTank, never()).insert(any(), anyInt(), any(), anyBoolean());
	}

	@Test
	void insertStorageOverflowReportsVoidedRemainderAsInsertedForPumpTransfers() {
		TankUpgradeWrapper tank = mock(TankUpgradeWrapper.class);
		when(tank.insert(eq(WATER), eq(1_000), any(), eq(false))).thenReturn(500);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.STORAGE_OVERFLOW);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(tank), List.of(voidUpgrade));
		ResourceHandler<FluidResource> source = mock();
		when(source.size()).thenReturn(1);
		when(source.getResource(0)).thenReturn(WATER);
		when(source.extract(eq(0), eq(WATER), anyInt(), any())).thenAnswer(invocation -> invocation.getArgument(2));

		int transferred = ResourceHandlerUtil.move(source, fluidHandler, resource -> true, 1_000, null);

		assertEquals(1_000, transferred);
	}

	private static int insert(BackpackFluidHandler fluidHandler) {
		try (Transaction tx = Transaction.openRoot()) {
			return fluidHandler.insert(0, WATER, 1_000, tx, false);
		}
	}

	private static BackpackFluidHandler fluidHandler(List<TankUpgradeWrapper> tanks, List<VoidUpgradeWrapper> voidUpgrades) {
		IStorageWrapper storageWrapper = mock(IStorageWrapper.class);
		UpgradeHandler upgradeHandler = mock(UpgradeHandler.class);
		when(storageWrapper.getUpgradeHandler()).thenReturn(upgradeHandler);
		when(upgradeHandler.getTypeWrappers(TankUpgradeItem.TYPE)).thenReturn(tanks);
		when(upgradeHandler.getTypeWrappers(VoidUpgradeItem.TYPE)).thenReturn(voidUpgrades);
		return new BackpackFluidHandler(storageWrapper);
	}

	private static VoidUpgradeWrapper mockVoidUpgrade(VoidType voidType) {
		VoidUpgradeWrapper voidUpgrade = mock(VoidUpgradeWrapper.class);
		when(voidUpgrade.shouldVoidFluid(any(), eq(voidType))).thenReturn(true);
		return voidUpgrade;
	}
}
