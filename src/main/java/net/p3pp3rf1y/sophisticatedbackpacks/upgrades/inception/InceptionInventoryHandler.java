package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.wrapper.CombinedInvWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.InsertResponseInventoryWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IObservableItemHandler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.IntConsumer;

public class InceptionInventoryHandler implements IObservableItemHandler {
	private IItemHandlerModifiable combinedInventories;
	private final ItemStack backpack;
	private final IObservableItemHandler wrappedInventoryHandler;
	private final InventoryOrder inventoryOrder;
	private final Map<Integer, IBackpackWrapper> subBackpacks = new HashMap<>();

	public InceptionInventoryHandler(ItemStack backpack, IObservableItemHandler wrappedInventoryHandler, InventoryOrder inventoryOrder) {
		this.backpack = backpack;
		this.wrappedInventoryHandler = wrappedInventoryHandler;
		this.inventoryOrder = inventoryOrder;
		wrappedInventoryHandler.addListener(this::onContentsChanged);

		refreshHandlerDelegate();
	}

	private void onContentsChanged(int slot) {
		LazyOptional<IBackpackWrapper> backpackWrapper = wrappedInventoryHandler.getStackInSlot(slot).getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
		if (subBackpacks.containsKey(slot) != backpackWrapper.isPresent() || backpackWrapper.map(w -> w != subBackpacks.get(slot)).orElse(false)) {
			refreshHandlerDelegate();
		}
	}

	private void refreshHandlerDelegate() {
		subBackpacks.clear();

		List<IItemHandlerModifiable> handlers = new ArrayList<>();
		if (inventoryOrder == InventoryOrder.MAIN_FIRST) {
			handlers.add(wrappedInventoryHandler);
		}

		for (int slot = 0; slot < wrappedInventoryHandler.getSlots(); slot++) {
			int finalSlot = slot;
			wrappedInventoryHandler.getStackInSlot(slot).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(wrapper -> {
						subBackpacks.put(finalSlot, wrapper);
						handlers.add(wrapper.getInventoryForInputOutput());
					});
		}
		if (inventoryOrder == InventoryOrder.INCEPTED_FIRST) {
			handlers.add(wrappedInventoryHandler);
		}
		combinedInventories = new CombinedInvWrapper(handlers.toArray(new IItemHandlerModifiable[] {}));
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
		ItemStack ret = InsertResponseInventoryWrapper.runOnBeforeInsert(slot, stack, simulate, this, backpack);
		if (ret.isEmpty()) {
			return ret;
		}

		ret = combinedInventories.insertItem(slot, stack, simulate);

		if (ret == stack) {
			return ret;
		}

		InsertResponseInventoryWrapper.runOnAfterInsert(slot, simulate, this, backpack);

		return ret;

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
	public void addListener(IntConsumer onContentsChanged) {
		wrappedInventoryHandler.addListener(onContentsChanged);
	}

	@Override
	public void clearListeners() {
		wrappedInventoryHandler.clearListeners();
	}
}
