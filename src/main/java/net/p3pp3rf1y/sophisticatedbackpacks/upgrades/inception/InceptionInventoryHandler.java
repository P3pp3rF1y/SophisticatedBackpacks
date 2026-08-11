package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemResourceHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

public class InceptionInventoryHandler implements ITrackedContentsItemResourceHandler {
	private final ITrackedContentsItemResourceHandler wrappedInventoryHandler;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;
	private List<ITrackedContentsItemResourceHandler> handlers;
	private int[] baseIndex;
	private int totalSize;

	public InceptionInventoryHandler(ITrackedContentsItemResourceHandler wrappedInventoryHandler, InventoryOrder inventoryOrder,
			SubBackpacksHandler subBackpacksHandler) {
		this.wrappedInventoryHandler = wrappedInventoryHandler;
		this.inventoryOrder = inventoryOrder;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(sbs -> refreshHandlerDelegate());

		refreshHandlerDelegate();
	}

	private void refreshHandlerDelegate() {
		handlers = new ArrayList<>();
		if (inventoryOrder == InventoryOrder.MAIN_FIRST) {
			handlers.add(wrappedInventoryHandler);
		}
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> handlers.add(sbp.getInventoryForInputOutput()));
		subBackpacksHandler.saveInitializedSubBackpacks();
		if (inventoryOrder == InventoryOrder.INCEPTED_FIRST) {
			handlers.add(wrappedInventoryHandler);
		}

		baseIndex = new int[handlers.size()];
		int index = 0;
		for (int i = 0; i < handlers.size(); i++) {
			index += handlers.get(i).size();
			baseIndex[i] = index;
		}
		totalSize = index;
	}

	private void refreshSubBackpacks() {
		subBackpacksHandler.refresh();
	}

	@Override
	public void setStackInSlot(int slot, ItemStack stack) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(slot);
		getHandlerFromIndex(handlerIndex).setStackInSlot(getSlotFromIndex(slot, handlerIndex), stack);
	}

	@Override
	public int size() {
		refreshSubBackpacks();
		return totalSize;
	}

	@Override
	public ItemResource getResource(int i) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(i);
		return getHandlerFromIndex(handlerIndex).getResource(getSlotFromIndex(i, handlerIndex));
	}

	@Override
	public long getAmountAsLong(int i) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(i);
		return getHandlerFromIndex(handlerIndex).getAmountAsLong(getSlotFromIndex(i, handlerIndex));
	}

	@Override
	public ItemStack getStackInSlot(int slot) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(slot);
		return getHandlerFromIndex(handlerIndex).getStackInSlot(getSlotFromIndex(slot, handlerIndex));
	}

	@Override
	public int insert(ItemResource resource, int amount, TransactionContext transaction) {
		refreshSubBackpacks();
		int inserted = 0;
		for (ITrackedContentsItemResourceHandler handler : handlers) {
			int r = handler.insert(resource, amount - inserted, transaction);
			inserted += r;
			if (inserted >= amount) {
				break;
			}
		}
		return inserted;
	}

	@Override
	public int insert(int index, ItemResource resource, int amount, TransactionContext tx) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(index);
		return getHandlerFromIndex(handlerIndex).insert(getSlotFromIndex(index, handlerIndex), resource, amount, tx);
	}

	@Override
	public int extract(int index, ItemResource resource, int amount, TransactionContext tx) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(index);
		return getHandlerFromIndex(handlerIndex).extract(getSlotFromIndex(index, handlerIndex), resource, amount, tx);
	}

	@Override
	public long getCapacityAsLong(int index, ItemResource resource) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(index);
		return getHandlerFromIndex(handlerIndex).getCapacityAsLong(getSlotFromIndex(index, handlerIndex), resource);
	}

	@Override
	public boolean isValid(int index, ItemResource resource) {
		refreshSubBackpacks();
		int handlerIndex = getHandlerIndex(index);
		return getHandlerFromIndex(handlerIndex).isValid(getSlotFromIndex(index, handlerIndex), resource);
	}

	@Override
	public Set<ItemStackKey> getTrackedStacks() {
		refreshSubBackpacks();
		Set<ItemStackKey> ret = new HashSet<>();
		handlers.forEach(h -> ret.addAll(h.getTrackedStacks()));
		return ret;
	}

	@Override
	public void registerTrackingListeners(Consumer<ItemStackKey> onAddStackKey, Consumer<ItemStackKey> onRemoveStackKey, Runnable onAddFirstEmptySlot,
			Runnable onRemoveLastEmptySlot) {
		refreshSubBackpacks();
		handlers.forEach(h -> h.registerTrackingListeners(onAddStackKey, onRemoveStackKey, onAddFirstEmptySlot, onRemoveLastEmptySlot));
	}

	@Override
	public void unregisterStackKeyListeners() {
		refreshSubBackpacks();
		handlers.forEach(ITrackedContentsItemResourceHandler::unregisterStackKeyListeners);
	}

	@Override
	public boolean hasEmptySlots() {
		refreshSubBackpacks();
		return handlers.stream().anyMatch(ITrackedContentsItemResourceHandler::hasEmptySlots);
	}

	@Override
	public int getInternalSlotLimit(int slot) {
		refreshSubBackpacks();
		int index = getHandlerIndex(slot);
		ITrackedContentsItemResourceHandler handler = getHandlerFromIndex(index);
		int localSlot = getSlotFromIndex(slot, index);
		return handler.getInternalSlotLimit(localSlot);
	}

	private int getHandlerIndex(int slot) {
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

	private int getSlotFromIndex(int slot, int index) {
		if (index <= 0 || index >= baseIndex.length) {
			return slot;
		}
		return slot - baseIndex[index - 1];
	}

	private ITrackedContentsItemResourceHandler getHandlerFromIndex(int index) {
		if (index < 0 || index >= handlers.size()) {
			return handlers.getFirst();
		}
		return handlers.get(index);
	}

	@Override
	public boolean isInsertBlocked() {
		refreshSubBackpacks();
		return handlers.stream().allMatch(ITrackedContentsItemResourceHandler::isInsertBlocked);
	}
}
