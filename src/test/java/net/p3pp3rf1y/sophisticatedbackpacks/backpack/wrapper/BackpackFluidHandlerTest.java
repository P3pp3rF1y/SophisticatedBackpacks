package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.world.level.material.Fluids;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.IFluidHandler.FluidAction;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class BackpackFluidHandlerTest {
	private static final FluidStack WATER = new FluidStack(Fluids.WATER, 1_000);

	@Test
	void fillAlwaysVoidsMatchingFluidBeforeFillingTanks() {
		TankUpgradeWrapper tank = mock(TankUpgradeWrapper.class);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.ALWAYS);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(tank), List.of(voidUpgrade));

		int filled = fluidHandler.fill(WATER, FluidAction.EXECUTE);

		assertEquals(WATER.getAmount(), filled);
		verify(tank, never()).fill(any(), any(), anyBoolean());
	}

	@Test
	void fillSlotOverflowFillsOnlyOneTankAndVoidsRemainder() {
		TankUpgradeWrapper firstTank = mock(TankUpgradeWrapper.class);
		TankUpgradeWrapper secondTank = mock(TankUpgradeWrapper.class);
		when(firstTank.getContents()).thenReturn(FluidStack.EMPTY);
		when(secondTank.getContents()).thenReturn(FluidStack.EMPTY);
		when(firstTank.fill(any(), eq(FluidAction.EXECUTE), eq(false))).thenReturn(500);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.SLOT_OVERFLOW);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(firstTank, secondTank), List.of(voidUpgrade));

		int filled = fluidHandler.fill(WATER, FluidAction.EXECUTE);

		assertEquals(WATER.getAmount(), filled);
		verify(firstTank).fill(WATER, FluidAction.EXECUTE, false);
		verify(secondTank, never()).fill(any(), any(), anyBoolean());
	}

	@Test
	void fillStorageOverflowReportsVoidedRemainderAsFilledForPumpTransfers() {
		TankUpgradeWrapper tank = mock(TankUpgradeWrapper.class);
		when(tank.fill(any(), eq(FluidAction.SIMULATE), eq(false))).thenAnswer(invocation -> invocation.getArgument(0, FluidStack.class).getAmount());
		when(tank.fill(any(), eq(FluidAction.EXECUTE), eq(false))).thenReturn(500);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.STORAGE_OVERFLOW);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(tank), List.of(voidUpgrade));
		FluidTank source = new FluidTank(WATER.getAmount());
		source.setFluid(WATER.copy());

		FluidStack transferred = FluidUtil.tryFluidTransfer(fluidHandler, source, WATER, true);

		assertEquals(WATER.getAmount(), transferred.getAmount());
		assertTrue(source.getFluid().isEmpty());
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
