package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.wrapper.CombinedInvWrapper;

import java.util.ArrayList;
import java.util.List;

public class InceptionInventoryHandler implements IItemHandlerModifiable {
	private IItemHandlerModifiable combinedInventories;
	private final IItemHandlerModifiable wrappedInventoryHandler;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;

	public InceptionInventoryHandler(IItemHandlerModifiable wrappedInventoryHandler, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
		this.wrappedInventoryHandler = wrappedInventoryHandler;
		this.inventoryOrder = inventoryOrder;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(sbs -> refreshHandlerDelegate());

		refreshHandlerDelegate();
	}

	private void refreshHandlerDelegate() {
		List<IItemHandlerModifiable> handlers = new ArrayList<>();
		if (inventoryOrder == InventoryOrder.MAIN_FIRST) {
			handlers.add(wrappedInventoryHandler);
		}
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> handlers.add(sbp.getInventoryForInputOutput()));
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
}
