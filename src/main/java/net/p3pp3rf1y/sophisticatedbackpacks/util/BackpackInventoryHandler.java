package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

public class BackpackInventoryHandler extends ItemStackHandler {
	private static final String INVENTORY_TAG = "inventory";
	private final ItemStack backpack;
	private final boolean persistent;

	public BackpackInventoryHandler(ItemStack backpack, boolean persistent) {
		super(getNumberOfSlots(backpack));
		this.backpack = backpack;
		this.persistent = persistent;
		NBTHelper.getCompound(backpack, INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	@Override
	public void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		if (persistent) {
			backpack.setTagInfo(INVENTORY_TAG, serializeNBT());
		}
	}

	private static int getNumberOfSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfSlots();
	}

	public void copyStacksTo(BackpackInventoryHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

}
