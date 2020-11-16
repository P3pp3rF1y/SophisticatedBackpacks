package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import java.util.function.Consumer;

public class BackpackInventoryHandler extends ItemStackHandler {
	private static final String INVENTORY_TAG = "inventory";
	private final ItemStack backpack;
	private final Consumer<ItemStack> backpackSaveHandler;

	public BackpackInventoryHandler(ItemStack backpack, Consumer<ItemStack> backpackSaveHandler) {
		super(getNumberOfSlots(backpack));
		this.backpack = backpack;
		this.backpackSaveHandler = backpackSaveHandler;
		NBTHelper.getCompound(backpack, INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	@Override
	public void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		saveInventory();
	}

	public void saveInventory() {
		backpack.setTagInfo(INVENTORY_TAG, serializeNBT());
		backpackSaveHandler.accept(backpack);
	}

	private static int getNumberOfSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfSlots();
	}

	public void copyStacksTo(BackpackInventoryHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}
}
