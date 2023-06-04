package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.wrapper.CombinedInvWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

public class InceptionInventoryHandler implements ITrackedContentsItemHandler {
	private IItemHandlerModifiable combinedInventories;
	private final ITrackedContentsItemHandler wrappedInventoryHandler;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;
	private List<ITrackedContentsItemHandler> handlers;
	private int[] baseIndex;

	public InceptionInventoryHandler(ITrackedContentsItemHandler wrappedInventoryHandler, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
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
		if (inventoryOrder == InventoryOrder.INCEPTED_FIRST) {
			handlers.add(wrappedInventoryHandler);
		}
		combinedInventories = new CombinedInvWrapper(handlers.toArray(new IItemHandlerModifiable[] {}));

		baseIndex = new int[handlers.size()];
		int index = 0;
		for (int i = 0; i < handlers.size(); i++) {
			index += handlers.get(i).getSlots();
			baseIndex[i] = index;
		}
	}

	@Override
	public void setStackInSlot(int slot, ItemStack stack) {
		combinedInventories.setStackInSlot(slot, stack);
	}

	@Override
	public int getSlots() {
		return combinedInventories.getSlots();
	}

	@Nonnull
	@Override
	public ItemStack getStackInSlot(int slot) {
		return combinedInventories.getStackInSlot(slot);
	}

	@Nonnull
	@Override
	public ItemStack insertItem(int slot, ItemStack stack, boolean simulate) {
		return combinedInventories.insertItem(slot, stack, simulate);
	}

	@Nonnull
	@Override
	public ItemStack extractItem(int slot, int amount, boolean simulate) {
		return combinedInventories.extractItem(slot, amount, simulate);
	}

	@Override
	public int getSlotLimit(int slot) {
		return combinedInventories.getSlotLimit(slot);
	}

	@Override
	public boolean isItemValid(int slot, ItemStack stack) {
		return combinedInventories.isItemValid(slot, stack);
	}

	@Override
	public ItemStack insertItem(ItemStack stack, boolean simulate) {
		ItemStack remainingStack = stack;
		for (IItemHandlerSimpleInserter handler : handlers) {
			remainingStack = handler.insertItem(remainingStack, simulate);
			if (remainingStack.isEmpty()) {
				break;
			}
		}

		return remainingStack;
	}

	@Override
	public Set<ItemStackKey> getTrackedStacks() {
		Set<ItemStackKey> ret = new HashSet<>();
		handlers.forEach(h -> ret.addAll(h.getTrackedStacks()));
		return ret;
	}

	@Override
	public void registerTrackingListeners(Consumer<ItemStackKey> onAddStackKey, Consumer<ItemStackKey> onRemoveStackKey, Runnable onAddFirstEmptySlot, Runnable onRemoveLastEmptySlot) {
		handlers.forEach(h -> h.registerTrackingListeners(onAddStackKey, onRemoveStackKey, onAddFirstEmptySlot, onRemoveLastEmptySlot));
	}

	@Override
	public void unregisterStackKeyListeners() {
		handlers.forEach(ITrackedContentsItemHandler::unregisterStackKeyListeners);
	}

	@Override
	public boolean hasEmptySlots() {
		return handlers.stream().anyMatch(ITrackedContentsItemHandler::hasEmptySlots);
	}

	@Override
	public int getInternalSlotLimit(int slot) {
		int index = getIndexForSlot(slot);
		ITrackedContentsItemHandler handler = getHandlerFromIndex(index);
		int localSlot = getSlotFromIndex(slot, index);
		return handler.getInternalSlotLimit(localSlot);
	}

	private int getIndexForSlot(int slot) {
		if (slot < 0) {return -1;}

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

	private ITrackedContentsItemHandler getHandlerFromIndex(int index) {
		if (index < 0 || index >= handlers.size()) {
			return handlers.get(0);
		}
		return handlers.get(index);
	}
}
