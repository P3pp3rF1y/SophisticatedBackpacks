package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeWrapper;

import java.util.List;

public class BackpackFluidHandler implements IStorageFluidHandler {
	private final IStorageWrapper backpackWrapper;

	public BackpackFluidHandler(IStorageWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	@Override
	public int size() {
		return getAllTanks().size();
	}

	@Override
	public FluidResource getResource(int index) {
		return isInvalidTank(index) ? FluidResource.EMPTY : getAllTanks().get(index).getResource();
	}

	@Override
	public long getAmountAsLong(int index) {
		return isInvalidTank(index) ? 0 : getAllTanks().get(index).getAmount();
	}

	private List<TankUpgradeWrapper> getAllTanks() {
		return backpackWrapper.getUpgradeHandler().getTypeWrappers(TankUpgradeItem.TYPE);
	}

	@Override
	public long getCapacityAsLong(int index, FluidResource resource) {
		return isInvalidTank(index) || !resource.matches(getAllTanks().get(index).getContents()) ? 0 : getAllTanks().get(index).getCapacity();
	}

	@Override
	public boolean isValid(int index, FluidResource resource) {
		if (isInvalidTank(index)) {
			return false;
		}

		FluidStack contents = getAllTanks().get(index).getContents();
		if (contents.isEmpty())
			return true;
		return resource.matches(contents);
	}

	@Override
	public int insert(int index, FluidResource resource, int amount, TransactionContext tx, boolean ignoreInOutLimit) {
		FluidStack fluid = resource.toStack(amount);
		if (shouldVoid(fluid, VoidType.ALWAYS)) {
			return amount;
		}

		if (shouldVoid(fluid, VoidType.SLOT_OVERFLOW)) {
			insertOneTank(resource, amount, tx, ignoreInOutLimit);
			return amount;
		}

		int inserted = insertAllTanks(resource, amount, tx, ignoreInOutLimit);
		return inserted == amount || !shouldVoid(fluid, VoidType.STORAGE_OVERFLOW) ? inserted : amount;
	}

	@Override
	public int extract(int index, FluidResource resource, int amount, TransactionContext tx, boolean ignoreInOutLimit) {
		int extracted = 0;
		int toExtract = amount;
		for (TankUpgradeWrapper tank : getAllTanks()) {
			extracted += tank.extract(resource, toExtract, tx, ignoreInOutLimit);
			if (extracted >= amount) {
				return amount;
			}
			toExtract = amount - extracted;
		}
		return extracted;
	}

	private boolean isInvalidTank(int tank) {
		return tank < 0 || tank >= size();
	}

	private int insertOneTank(FluidResource resource, int amount, TransactionContext tx, boolean ignoreInOutLimit) {
		for (TankUpgradeWrapper tank : getAllTanks()) {
			if (resource.matches(tank.getContents())) {
				return tank.insert(resource, amount, tx, ignoreInOutLimit);
			}
		}
		for (TankUpgradeWrapper tank : getAllTanks()) {
			if (tank.getContents().isEmpty()) {
				return tank.insert(resource, amount, tx, ignoreInOutLimit);
			}
		}
		return 0;
	}

	private int insertAllTanks(FluidResource resource, int amount, TransactionContext tx, boolean ignoreInOutLimit) {
		int inserted = 0;
		int toInsert = amount;
		for (TankUpgradeWrapper tank : getAllTanks()) {
			inserted += tank.insert(resource, toInsert, tx, ignoreInOutLimit);
			if (inserted >= amount) {
				return amount;
			}
			toInsert = amount - inserted;
		}
		return inserted;
	}

	private boolean shouldVoid(FluidStack fluid, VoidType voidType) {
		for (VoidUpgradeWrapper voidUpgrade : backpackWrapper.getUpgradeHandler().getTypeWrappers(VoidUpgradeItem.TYPE)) {
			if (voidUpgrade.shouldVoidFluid(fluid, voidType)) {
				return true;
			}
		}
		return false;
	}
}
