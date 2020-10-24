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
		NBTHelper.getCompound(backpack, INVENTORY_TAG).ifPresent(this::deserializeNBT);
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
		InventoryHelper.copyTo(this, otherHandler);
	}

}
