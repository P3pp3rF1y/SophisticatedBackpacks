package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;

import java.util.List;

public class InsertResponseInventoryWrapper implements IItemHandlerModifiable {
	private final IBackpackWrapper backpackWrapper;
	private final IItemHandlerModifiable inventory;

	public InsertResponseInventoryWrapper(IBackpackWrapper backpackWrapper, IItemHandlerModifiable inventory) {
		this.backpackWrapper = backpackWrapper;
		this.inventory = inventory;
	}

	@Override
	public void setStackInSlot(int slot, ItemStack stack) {
		inventory.setStackInSlot(slot, stack);
	}

	@Override
	public int getSlots() {
		return inventory.getSlots();
	}

	@Override
	public ItemStack getStackInSlot(int slot) {
		return inventory.getStackInSlot(slot);
	}

	@Override
	public ItemStack insertItem(int slot, ItemStack stack, boolean simulate) {
		ItemStack ret = runOnBeforeInsert(slot, stack, simulate, this, backpackWrapper);
		if (ret.isEmpty()) {
			return ret;
		}

		ret = inventory.insertItem(slot, ret, simulate);

		if (ret == stack) {
			return ret;
		}

		runOnAfterInsert(slot, simulate, this, backpackWrapper);

		return ret;
	}

	@Override
	public ItemStack extractItem(int slot, int amount, boolean simulate) {
		return inventory.extractItem(slot, amount, simulate);
	}

	@Override
	public int getSlotLimit(int slot) {
		return inventory.getSlotLimit(slot);
	}

	@Override
	public boolean isItemValid(int slot, ItemStack stack) {
		return inventory.isItemValid(slot, stack);
	}

	private void runOnAfterInsert(int slot, boolean simulate, IItemHandler handler, IBackpackWrapper backpackWrapper) {
		if (!simulate) {
			backpackWrapper.getUpgradeHandler().getWrappersThatImplementFromMainBackpack(IInsertResponseUpgrade.class).forEach(u -> u.onAfterInsert(handler, slot));
		}
	}

	private ItemStack runOnBeforeInsert(int slot, ItemStack stack, boolean simulate, IItemHandler handler, IBackpackWrapper backpackWrapper) {
		List<IInsertResponseUpgrade> wrappers = backpackWrapper.getUpgradeHandler().getWrappersThatImplementFromMainBackpack(IInsertResponseUpgrade.class);
		ItemStack remaining = stack;
		for (IInsertResponseUpgrade upgrade : wrappers) {
			remaining = upgrade.onBeforeInsert(handler, slot, remaining, simulate);
			if (remaining.isEmpty()) {
				return ItemStack.EMPTY;
			}
		}
		return remaining;
	}
}
