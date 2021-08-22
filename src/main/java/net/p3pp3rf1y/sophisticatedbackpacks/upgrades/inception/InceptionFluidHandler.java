package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.fluids.capability.templates.EmptyFluidHandler;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

public class InceptionFluidHandler implements IFluidHandlerItem {
	@Nullable
	private final IFluidHandlerItem wrappedFluidHandler;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;
	private IFluidHandler[] fluidHandlers;
	protected int[] baseIndex;
	protected int tankCount;
	private final ItemStack backpack;

	public InceptionFluidHandler(
			@Nullable IFluidHandlerItem wrappedFluidHandler, ItemStack backpack, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
		this.wrappedFluidHandler = wrappedFluidHandler;
		this.backpack = backpack;
		this.inventoryOrder = inventoryOrder;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(sbs -> refreshHandlers());
		refreshHandlers();
	}

	private void refreshHandlers() {
		List<IFluidHandler> handlers = new ArrayList<>();
		if (wrappedFluidHandler != null && inventoryOrder == InventoryOrder.MAIN_FIRST) {
			handlers.add(wrappedFluidHandler);
		}
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> sbp.getFluidHandler().ifPresent(handlers::add));
		if (wrappedFluidHandler != null && inventoryOrder == InventoryOrder.INCEPTED_FIRST) {
			handlers.add(wrappedFluidHandler);
		}
		fluidHandlers = handlers.toArray(new IFluidHandler[] {});
		baseIndex = new int[fluidHandlers.length];
		int index = 0;
		for (int i = 0; i < fluidHandlers.length; i++) {
			index += fluidHandlers[i].getTanks();
			baseIndex[i] = index;
		}
		tankCount = index;
	}

	@Override
	public int getTanks() {
		return tankCount;
	}

	private int getHandlerIndexForTank(int slot) {
		if (slot < 0) {
			return -1;
		}

		for (int i = 0; i < baseIndex.length; i++) {
			if (slot - baseIndex[i] < 0) {
				return i;
			}
		}
		return -1;
	}

	protected IFluidHandler getHandlerFromIndex(int index) {
		if (index < 0 || index >= fluidHandlers.length) {
			return EmptyFluidHandler.INSTANCE;
		}
		return fluidHandlers[index];
	}

	protected int getTankFromIndex(int tank, int handlerIndex) {
		if (handlerIndex <= 0 || handlerIndex >= baseIndex.length) {
			return tank;
		}
		return tank - baseIndex[handlerIndex - 1];
	}

	private <T> T getFluidHandlerValue(int overallTank, BiFunction<IFluidHandler, Integer, T> getValue) {
		int handlerIndex = getHandlerIndexForTank(overallTank);
		return getValue.apply(getHandlerFromIndex(handlerIndex), getTankFromIndex(overallTank, handlerIndex));
	}

	@Nonnull
	@Override
	public FluidStack getFluidInTank(int tank) {
		return getFluidHandlerValue(tank, IFluidHandler::getFluidInTank);
	}

	@Override
	public int getTankCapacity(int tank) {
		return getFluidHandlerValue(tank, IFluidHandler::getTankCapacity);
	}

	@Override
	public boolean isFluidValid(int tank, @Nonnull FluidStack stack) {
		return getFluidHandlerValue(tank, (h, t) -> h.isFluidValid(t, stack));
	}

	@Override
	public int fill(FluidStack resource, FluidAction action) {
		int filled = 0;
		FluidStack toFill = resource;
		for (IFluidHandler fluidHandler : fluidHandlers) {
			filled += fluidHandler.fill(toFill, action);
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
		FluidStack toDrain = resource;
		for (IFluidHandler fluidHandler : fluidHandlers) {
			drained += fluidHandler.drain(toDrain, action).getAmount();
			if (drained == resource.getAmount()) {
				return resource;
			}
			toDrain = new FluidStack(toDrain.getFluid(), resource.getAmount() - drained);
		}

		return drained == 0 ? FluidStack.EMPTY : new FluidStack(resource.getFluid(), drained);
	}

	@Nonnull
	@Override
	public FluidStack drain(int maxDrain, FluidAction action) {
		for (IFluidHandler fluidHandler : fluidHandlers) {
			FluidStack drained = fluidHandler.drain(maxDrain, action);
			if (!drained.isEmpty()) {
				return drained;
			}
		}
		return FluidStack.EMPTY;
	}

	@Nonnull
	@Override
	public ItemStack getContainer() {
		return backpack;
	}
}
