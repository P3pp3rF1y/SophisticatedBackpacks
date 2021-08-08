package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeWrapper;

import javax.annotation.Nonnull;
import java.util.List;

public class BackpackFluidHandler implements IFluidHandlerItem {
	private final IBackpackWrapper backpackWrapper;

	public BackpackFluidHandler(IBackpackWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	@Override
	public int getTanks() {
		return getAllTanks().size();
	}

	@Nonnull
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
	public int fill(FluidStack resource, FluidAction action) {
		int filled = 0;
		FluidStack toFill = resource;
		for (TankUpgradeWrapper tank : getAllTanks()) {
			filled += tank.fill(toFill, action);
			if (filled == resource.getAmount()) {
				return resource.getAmount();
			}
			toFill = new FluidStack(toFill.getFluid(), resource.getAmount() - filled);
		}

		return filled;
	}

	@Nonnull
	@Override
	public FluidStack drain(FluidStack resource, FluidAction action) {
		int drained = 0;
		int toDrain = resource.getAmount();
		for (TankUpgradeWrapper tank : getAllTanks()) {
			drained += tank.drain(toDrain, action).getAmount();
			if (drained == resource.getAmount()) {
				return resource;
			}
			toDrain = resource.getAmount() - drained;
		}

		return drained == 0 ? FluidStack.EMPTY : new FluidStack(resource.getFluid(), drained);
	}

	@Nonnull
	@Override
	public FluidStack drain(int maxDrain, FluidAction action) {
		for (TankUpgradeWrapper tank : getAllTanks()) {
			FluidStack drained = tank.drain(maxDrain, action);
			if (!drained.isEmpty()) {
				return drained;
			}
		}
		return FluidStack.EMPTY;
	}

	private boolean isInvalidTank(int tank) {
		return tank < 0 || tank >= getTanks();
	}

	@Nonnull
	@Override
	public ItemStack getContainer() {
		return backpackWrapper.getBackpack();
	}
}
