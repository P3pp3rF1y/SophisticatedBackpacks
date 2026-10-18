package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.neoforged.neoforge.transfer.EmptyResourceHandler;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Consumer;

public class InceptionFluidHandler implements IStorageFluidHandler, InceptionHandler {
	@Nullable
	private final IStorageFluidHandler wrappedFluidHandler;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;
	private IStorageFluidHandler[] fluidHandlers;
	protected int[] baseIndex;
	protected int tankCount;
	private boolean handlersInvalidated;
	private final Consumer<Collection<IStorageWrapper>> refreshListener = subBackpacks -> refreshHandlers();
	private final Runnable upgradeCachesInvalidatedListener = () -> handlersInvalidated = true;

	public InceptionFluidHandler(@Nullable IStorageFluidHandler wrappedFluidHandler, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
		this.wrappedFluidHandler = wrappedFluidHandler;
		this.inventoryOrder = inventoryOrder;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(refreshListener);
		subBackpacksHandler.addUpgradeCachesInvalidatedListener(upgradeCachesInvalidatedListener);
		refreshHandlers();
	}

	@Override
	public void onBeforeDeconstruct() {
		subBackpacksHandler.removeRefreshListener(refreshListener);
		subBackpacksHandler.removeUpgradeCachesInvalidatedListener(upgradeCachesInvalidatedListener);
	}

	private void refreshHandlers() {
		List<IStorageFluidHandler> handlers = new ArrayList<>();
		if (wrappedFluidHandler != null && inventoryOrder == InventoryOrder.MAIN_FIRST) {
			handlers.add(wrappedFluidHandler);
		}
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> sbp.getFluidHandler().ifPresent(handlers::add));
		if (wrappedFluidHandler != null && inventoryOrder == InventoryOrder.INCEPTED_FIRST) {
			handlers.add(wrappedFluidHandler);
		}
		fluidHandlers = handlers.toArray(new IStorageFluidHandler[]{});
		baseIndex = new int[fluidHandlers.length];
		int index = 0;
		for (int i = 0; i < fluidHandlers.length; i++) {
			index += fluidHandlers[i].size();
			baseIndex[i] = index;
		}
		tankCount = index;
		handlersInvalidated = false;
	}

	private void refreshHandlersIfNeeded() {
		if (handlersInvalidated) {
			refreshHandlers();
		}
	}

	@Override
	public int size() {
		refreshHandlersIfNeeded();
		return tankCount;
	}

	private int getHandlerIndexForTank(int slot) {
		refreshHandlersIfNeeded();
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

	protected ResourceHandler<FluidResource> getHandlerFromIndex(int index) {
		if (index < 0 || index >= fluidHandlers.length) {
			return EmptyResourceHandler.instance();
		}
		return fluidHandlers[index];
	}

	protected int getIndexInHandler(int overallIndex, int handlerIndex) {
		if (handlerIndex <= 0 || handlerIndex >= baseIndex.length) {
			return overallIndex;
		}
		return overallIndex - baseIndex[handlerIndex - 1];
	}

	private <T> T getFluidHandlerValue(int overallTank, BiFunction<ResourceHandler<FluidResource>, Integer, T> getValue) {
		int handlerIndex = getHandlerIndexForTank(overallTank);
		return getValue.apply(getHandlerFromIndex(handlerIndex), getIndexInHandler(overallTank, handlerIndex));
	}

	@Override
	public FluidResource getResource(int index) {
		return getFluidHandlerValue(index, ResourceHandler::getResource);
	}

	@Override
	public long getAmountAsLong(int i) {
		return getFluidHandlerValue(i, ResourceHandler::getAmountAsLong);
	}

	@Override
	public long getCapacityAsLong(int index, FluidResource resource) {
		return getFluidHandlerValue(index, (h, i) -> h.getCapacityAsLong(i, resource));
	}

	@Override
	public boolean isValid(int i, FluidResource resource) {
		return getFluidHandlerValue(i, (h, t) -> h.isValid(t, resource));
	}

	@Override
	public int insert(int index, FluidResource resource, int amount, TransactionContext tx, boolean ignoreInOutLimit) {
		int handlerIndex = getHandlerIndexForTank(index);
		if (handlerIndex < 0 || handlerIndex >= fluidHandlers.length) {
			return 0;
		}
		return fluidHandlers[handlerIndex].insert(getIndexInHandler(index, handlerIndex), resource, amount, tx, ignoreInOutLimit);
	}

	@Override
	public int extract(int index, FluidResource resource, int amount, TransactionContext tx, boolean ignoreInOutLimit) {
		int handlerIndex = getHandlerIndexForTank(index);
		if (handlerIndex < 0 || handlerIndex >= fluidHandlers.length) {
			return 0;
		}
		return fluidHandlers[handlerIndex].extract(getIndexInHandler(index, handlerIndex), resource, amount, tx, ignoreInOutLimit);
	}
}
