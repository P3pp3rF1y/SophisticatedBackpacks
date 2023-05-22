package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.tags.TagKey;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeWrapper;

import javax.annotation.Nonnull;
import java.util.List;

public class BackpackFluidHandler implements IStorageFluidHandler {
	private final IStorageWrapper backpackWrapper;

	public BackpackFluidHandler(IStorageWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	@Override
	public int getTanks() {
		return getAllTanks().size();
	}

	@Override
	public FluidStack getFluidInTank(int tank) {
		return isInvalidTank(tank) ? FluidStack.EMPTY : getAllTanks().get(tank).getContents();
	}

	@Nonnull
	private List<TankUpgradeWrapper> getAllTanks() {
		return backpackWrapper.getUpgradeHandler().getTypeWrappers(TankUpgradeItem.TYPE);
	}

	@Override
	public int getTankCapacity(int tank) {
		return isInvalidTank(tank) ? 0 : getAllTanks().get(tank).getTankCapacity();
	}

	@Override
	public boolean isFluidValid(int tank, @Nonnull FluidStack stack) {
		if (isInvalidTank(tank)) {
			return false;
		}

		FluidStack contents = getAllTanks().get(tank).getContents();
		return contents.isEmpty() || contents.isFluidEqual(stack);
	}

	@Override
	public int fill(FluidStack resource, FluidAction action, boolean ignoreInOutLimit) {
		int filled = 0;
		FluidStack toFill = resource;
		for (TankUpgradeWrapper tank : getAllTanks()) {
			filled += tank.fill(toFill, action, ignoreInOutLimit);
			if (filled == resource.getAmount()) {
				return resource.getAmount();
			}
			toFill = new FluidStack(toFill, resource.getAmount() - filled);
		}

		return filled;

	}

	@Override
	public int fill(FluidStack resource, FluidAction action) {
		return fill(resource, action, false);
	}

	@Override
	public FluidStack drain(TagKey<Fluid> resourceTag, int maxDrain, FluidAction action, boolean ignoreInOutLimit) {
		FluidStack drained = FluidStack.EMPTY;
		int toDrain = maxDrain;
		for (TankUpgradeWrapper tank : getAllTanks()) {
			Fluid tankFluid = tank.getContents().getFluid();
			if ((drained.isEmpty() && tankFluid.is(resourceTag)) || tank.getContents().isFluidEqual(drained)) {
				if (drained.isEmpty()) {
					drained = tank.drain(toDrain, action, ignoreInOutLimit);
				} else {
					drained.grow(tank.drain(toDrain, action, ignoreInOutLimit).getAmount());
				}

				if (drained.getAmount() == maxDrain) {
					return drained;
				}

				toDrain = maxDrain - drained.getAmount();
			}
		}

		return drained;
	}

	@Override
	public FluidStack drain(FluidStack resource, FluidAction action, boolean ignoreInOutLimit) {
		int drained = 0;
		int toDrain = resource.getAmount();
		for (TankUpgradeWrapper tank : getAllTanks()) {
			if (tank.getContents().isFluidEqual(resource)) {
				drained += tank.drain(toDrain, action, ignoreInOutLimit).getAmount();
				if (drained == resource.getAmount()) {
					return resource;
				}
				toDrain = resource.getAmount() - drained;
			}
		}

		return drained == 0 ? FluidStack.EMPTY : new FluidStack(resource, drained);
	}

	@Override
	public FluidStack drain(FluidStack resource, FluidAction action) {
		return drain(resource, action, false);
	}

	@Override
	public FluidStack drain(int maxDrain, FluidAction action, boolean ignoreInOutLimit) {
		for (TankUpgradeWrapper tank : getAllTanks()) {
			FluidStack drained = tank.drain(maxDrain, action, ignoreInOutLimit);
			if (!drained.isEmpty()) {
				return drained;
			}
		}
		return FluidStack.EMPTY;
	}

	@Override
	public FluidStack drain(int maxDrain, FluidAction action) {
		return drain(maxDrain, action, false);
	}

	private boolean isInvalidTank(int tank) {
		return tank < 0 || tank >= getTanks();
	}

	@Override
	public ItemStack getContainer() {
		return backpackWrapper.getWrappedStorageStack();
	}
}
