package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.items.IItemHandlerModifiable;
import net.neoforged.neoforge.items.wrapper.CombinedInvWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;

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
		combinedInventories = new CombinedInvWrapper(handlers.toArray(new IItemHandlerModifiable[]{})) {
			@Override
			public ItemStack insertItem(int slot, ItemStack stack, boolean simulate) {
				ItemStack remaining = stack;
				for (ITrackedContentsItemHandler handler : handlers) {
					remaining = handler.insertItem(remaining, simulate);
					if (remaining.isEmpty()) {
						break;
					}
				}
				return remaining;
			}
		};
	}

	@Override
	public void setStackInSlot(int slot, ItemStack stack) {
		combinedInventories.setStackInSlot(slot, stack);
	}

	@Override
	public int getSlots() {
		return combinedInventories.getSlots();
	}

	@Override
	public ItemStack getStackInSlot(int slot) {
		return combinedInventories.getStackInSlot(slot);
	}

	@Override
	public ItemStack insertItem(int slot, ItemStack stack, boolean simulate) {
		return combinedInventories.insertItem(slot, stack, simulate);
	}

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
	public boolean isInsertBlocked() {
		return handlers.stream().allMatch(ITrackedContentsItemHandler::isInsertBlocked);
	}

	@Override
	public ItemStack extractItem(ItemStack stack, boolean simulate) {
		ItemStack remaining = stack;
		for (ITrackedContentsItemHandler handler : handlers) {
			ItemStack extracted = handler.extractItem(remaining, simulate);
			if (extracted.getCount() > 0) {
				remaining = stack.copyWithCount(remaining.getCount() - extracted.getCount());
			}
			if (remaining.isEmpty()) {
				break;
			}
		}

		return stack.getCount() == remaining.getCount() ? ItemStack.EMPTY : stack.copyWithCount(stack.getCount() - remaining.getCount());
	}
}
