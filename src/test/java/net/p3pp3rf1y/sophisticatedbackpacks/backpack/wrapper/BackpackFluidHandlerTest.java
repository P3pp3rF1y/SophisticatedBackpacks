package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.level.material.Fluids;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.IFluidHandler.FluidAction;
import net.minecraftforge.fluids.capability.templates.FluidTank;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeWrapper;
import org.junit.jupiter.api.BeforeAll;
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
	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@Test
	void fillAlwaysVoidsMatchingFluidBeforeFillingTanks() {
		FluidStack water = water();
		TankUpgradeWrapper tank = mock(TankUpgradeWrapper.class);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.ALWAYS);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(tank), List.of(voidUpgrade));

		int filled = fluidHandler.fill(water, FluidAction.EXECUTE);

		assertEquals(water.getAmount(), filled);
		verify(tank, never()).fill(any(), any(), anyBoolean());
	}

	@Test
	void fillSlotOverflowFillsOnlyOneTankAndVoidsRemainder() {
		FluidStack water = water();
		TankUpgradeWrapper firstTank = mock(TankUpgradeWrapper.class);
		TankUpgradeWrapper secondTank = mock(TankUpgradeWrapper.class);
		when(firstTank.getContents()).thenReturn(FluidStack.EMPTY);
		when(secondTank.getContents()).thenReturn(FluidStack.EMPTY);
		when(firstTank.fill(any(), eq(FluidAction.EXECUTE), eq(false))).thenReturn(500);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.SLOT_OVERFLOW);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(firstTank, secondTank), List.of(voidUpgrade));

		int filled = fluidHandler.fill(water, FluidAction.EXECUTE);

		assertEquals(water.getAmount(), filled);
		verify(firstTank).fill(water, FluidAction.EXECUTE, false);
		verify(secondTank, never()).fill(any(), any(), anyBoolean());
	}

	@Test
	void fillStorageOverflowReportsVoidedRemainderAsFilledForPumpTransfers() {
		FluidStack water = water();
		TankUpgradeWrapper tank = mock(TankUpgradeWrapper.class);
		when(tank.fill(any(), eq(FluidAction.SIMULATE), eq(false))).thenAnswer(invocation -> invocation.getArgument(0, FluidStack.class).getAmount());
		when(tank.fill(any(), eq(FluidAction.EXECUTE), eq(false))).thenReturn(500);
		VoidUpgradeWrapper voidUpgrade = mockVoidUpgrade(VoidType.STORAGE_OVERFLOW);
		BackpackFluidHandler fluidHandler = fluidHandler(List.of(tank), List.of(voidUpgrade));
		FluidTank source = new FluidTank(water.getAmount());
		source.setFluid(water.copy());

		FluidStack transferred = FluidUtil.tryFluidTransfer(fluidHandler, source, water, true);

		assertEquals(water.getAmount(), transferred.getAmount());
		assertTrue(source.getFluid().isEmpty());
	}

	private static FluidStack water() {
		return new FluidStack(Fluids.WATER, 1_000);
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
