package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.tags.Tag;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackFluidHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeWrapper;

import javax.annotation.Nonnull;
import java.util.List;

public class BackpackFluidHandler implements IBackpackFluidHandler {
	private final IBackpackWrapper backpackWrapper;

	public BackpackFluidHandler(IBackpackWrapper backpackWrapper) {
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
		return contents.isEmpty() || contents.getFluid() == stack.getFluid();
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
			toFill = new FluidStack(toFill.getFluid(), resource.getAmount() - filled);
		}

		return filled;

	}

	@Override
	public int fill(FluidStack resource, FluidAction action) {
		return fill(resource, action, false);
	}

	@Override
	public FluidStack drain(Tag<Fluid> resourceTag, int maxDrain, FluidAction action, boolean ignoreInOutLimit) {
		FluidStack drained = FluidStack.EMPTY;
		int toDrain = maxDrain;
		for (TankUpgradeWrapper tank : getAllTanks()) {
			Fluid tankFluid = tank.getContents().getFluid();
			if ((drained.isEmpty() && tankFluid.is(resourceTag)) || tankFluid == drained.getFluid()) {
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
			if (tank.getContents().getFluid() == resource.getFluid()) {
				drained += tank.drain(toDrain, action, ignoreInOutLimit).getAmount();
				if (drained == resource.getAmount()) {
					return resource;
				}
				toDrain = resource.getAmount() - drained;
			}
		}

		return drained == 0 ? FluidStack.EMPTY : new FluidStack(resource.getFluid(), drained);
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
		return backpackWrapper.getBackpack();
	}
}
