package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.wrapper.CombinedInvWrapper;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InceptionInventoryHandler implements IItemHandlerModifiable {
	private IItemHandlerModifiable invHandlerDelegate;
	private final ItemStack backpack;
	private final BackpackInventoryHandler backpackInventoryHandler;
	private final Map<Integer, IBackpackWrapper> subBackpacks = new HashMap<>();

	public InceptionInventoryHandler(ItemStack backpack, BackpackInventoryHandler backpackInventoryHandler) {
		this.backpack = backpack;
		this.backpackInventoryHandler = backpackInventoryHandler;
		backpackInventoryHandler.setParentContentsChangeHandler(this::onContentsChanged);

		refreshHandlerDelegate();
	}

	private void onContentsChanged(int slot) {
		LazyOptional<IBackpackWrapper> backpackWrapper = backpackInventoryHandler.getStackInSlot(slot).getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY);
		if (subBackpacks.containsKey(slot) != backpackWrapper.isPresent() || backpackWrapper.map(w -> w != subBackpacks.get(slot)).orElse(false)) {
			refreshHandlerDelegate();
		}
	}

	private void refreshHandlerDelegate() {
		subBackpacks.clear();

		List<IItemHandlerModifiable> handlers = new ArrayList<>();
		handlers.add(backpackInventoryHandler);

		for (int slot = 0; slot < backpackInventoryHandler.getSlots(); slot++) {
			int finalSlot = slot;
			backpackInventoryHandler.getStackInSlot(slot).getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
					.ifPresent(wrapper -> {
						subBackpacks.put(finalSlot, wrapper);
						handlers.add(wrapper.getFilteredHandler());
					});
		}

		invHandlerDelegate = new CombinedInvWrapper(handlers.toArray(new IItemHandlerModifiable[] {}));
	}

	@Override
	public void setStackInSlot(int slot, @Nonnull ItemStack stack) {
		invHandlerDelegate.setStackInSlot(slot, stack);
	}

	@Override
	public int getSlots() {
		return invHandlerDelegate.getSlots();
	}

	@Override
	public ItemStack getStackInSlot(int slot) {
		return invHandlerDelegate.getStackInSlot(slot);
	}

	@Override
	public ItemStack insertItem(int slot, @Nonnull ItemStack stack, boolean simulate) {
		ItemStack ret = InsertResponseHelper.runOnBeforeInsert(slot, stack, simulate, this, backpack);
		if (ret.isEmpty()) {
			return ret;
		}

		ret = invHandlerDelegate.insertItem(slot, stack, simulate);

		if (ret == stack) {
			return ret;
		}

		InsertResponseHelper.runOnAfterInsert(slot, simulate, this, backpack);

		return ret;

	}

	@Override
	public ItemStack extractItem(int slot, int amount, boolean simulate) {
		return invHandlerDelegate.extractItem(slot, amount, simulate);
	}

	@Override
	public int getSlotLimit(int slot) {
		return invHandlerDelegate.getSlotLimit(slot);
	}

	@Override
	public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
		return invHandlerDelegate.isItemValid(slot, stack);
	}
}
