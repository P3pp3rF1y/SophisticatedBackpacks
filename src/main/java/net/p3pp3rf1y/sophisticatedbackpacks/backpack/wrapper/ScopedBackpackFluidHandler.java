package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.IFluidHandlerItem;

import java.util.function.Supplier;

/** A fluid capability on a copied stack must not permanently take ownership of the cached backpack wrapper. */
class ScopedBackpackFluidHandler implements IFluidHandlerItem {
	private final ItemStack container;
	private final BackpackWrapper wrapper;
	private final IFluidHandler delegate;

	ScopedBackpackFluidHandler(ItemStack container, BackpackWrapper wrapper, IFluidHandler delegate) {
		this.container = container;
		this.wrapper = wrapper;
		this.delegate = delegate;
	}

	private <T> T onContainer(Supplier<T> operation) {
		ItemStack original = wrapper.getBackpack();
		if (original == container) {
			return operation.get();
		}
		wrapper.setBackpackStack(container);
		try {
			return operation.get();
		} finally {
			wrapper.setBackpackStack(original);
		}
	}

	@Override
	public ItemStack getContainer() {
		return container;
	}

	@Override
	public int getTanks() {
		return onContainer(delegate::getTanks);
	}

	@Override
	public FluidStack getFluidInTank(int tank) {
		return onContainer(() -> delegate.getFluidInTank(tank));
	}

	@Override
	public int getTankCapacity(int tank) {
		return onContainer(() -> delegate.getTankCapacity(tank));
	}

	@Override
	public boolean isFluidValid(int tank, FluidStack stack) {
		return onContainer(() -> delegate.isFluidValid(tank, stack));
	}

	@Override
	public int fill(FluidStack resource, FluidAction action) {
		return onContainer(() -> delegate.fill(resource, action));
	}

	@Override
	public FluidStack drain(FluidStack resource, FluidAction action) {
		return onContainer(() -> delegate.drain(resource, action));
	}

	@Override
	public FluidStack drain(int maxDrain, FluidAction action) {
		return onContainer(() -> delegate.drain(maxDrain, action));
	}
}
