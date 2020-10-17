package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

public class BackpackInventoryHandler extends ItemStackHandler {
	private static final String INVENTORY_TAG = "inventory";
	private final ItemStack backpack;

	public BackpackInventoryHandler(ItemStack backpack) {
		super(getNumberOfSlots(backpack));
		this.backpack = backpack;
		if (backpack.hasTag()) {
			//noinspection ConstantConditions
			deserializeNBT(backpack.getTag().getCompound(INVENTORY_TAG) );
		}
	}

	@Override
	protected void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		backpack.setTagInfo(INVENTORY_TAG, serializeNBT());
	}

	private static int getNumberOfSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfSlots();
	}

	public void copyStacksTo(BackpackInventoryHandler otherHandler) {
		for(int slot = 0; slot < getSlots() && slot < otherHandler.getSlots(); slot++) {
			ItemStack slotStack = getStackInSlot(slot);
			if (!slotStack.isEmpty()) {
				otherHandler.setStackInSlot(slot, slotStack);
			}
		}
	}
}
