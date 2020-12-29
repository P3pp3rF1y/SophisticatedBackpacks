package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;

import java.util.List;

public class InsertResponseInventoryWrapper implements IItemHandlerModifiable {
	private final ItemStack backpack;
	private final IItemHandlerModifiable inventory;

	public InsertResponseInventoryWrapper(ItemStack backpack, IItemHandlerModifiable inventory) {
		this.backpack = backpack;
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
		ItemStack ret = runOnBeforeInsert(slot, stack, simulate, this, backpack);
		if (ret.isEmpty()) {
			return ret;
		}

		ret = inventory.insertItem(slot, ret, simulate);

		if (ret == stack) {
			return ret;
		}

		runOnAfterInsert(slot, simulate, this, backpack);

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

	public static void runOnAfterInsert(int slot, boolean simulate, IItemHandler handler, ItemStack backpack) {
		if (!simulate) {
			backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(IInsertResponseUpgrade.class)
							.forEach(u -> u.onAfterInsert(handler, slot)));
		}
	}

	public static ItemStack runOnBeforeInsert(int slot, ItemStack stack, boolean simulate, IItemHandler handler, ItemStack backpack) {
		return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> {
					List<IInsertResponseUpgrade> wrappers = wrapper.getUpgradeHandler().getWrappersThatImplement(IInsertResponseUpgrade.class);
					ItemStack remaining = stack;
					for (IInsertResponseUpgrade upgrade : wrappers) {
						remaining = upgrade.onBeforeInsert(handler, slot, remaining, simulate);
						if (remaining.isEmpty()) {
							return ItemStack.EMPTY;
						}
					}
					return remaining;
				}).orElse(stack);
	}
}
