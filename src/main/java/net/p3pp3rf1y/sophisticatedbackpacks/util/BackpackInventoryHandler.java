package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class BackpackInventoryHandler extends ItemStackHandler {
	private static final String INVENTORY_TAG = "inventory";
	private final ItemStack backpack;
	private final Consumer<ItemStack> backpackSaveHandler;
	private final BiConsumer<Integer, Supplier<ItemStack>> notificationHandler;

	public BackpackInventoryHandler(ItemStack backpack, Consumer<ItemStack> backpackSaveHandler, BiConsumer<Integer, Supplier<ItemStack>> notificationHandler) {
		super(getNumberOfSlots(backpack));
		this.backpack = backpack;
		this.backpackSaveHandler = backpackSaveHandler;
		this.notificationHandler = notificationHandler;
		NBTHelper.getCompound(backpack, INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	@Override
	public void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		backpack.setTagInfo(INVENTORY_TAG, serializeNBT());
		backpackSaveHandler.accept(backpack);
		notificationHandler.accept(slot, () -> getStackInSlot(slot));
	}

	private static int getNumberOfSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfSlots();
	}

	public void copyStacksTo(BackpackInventoryHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

	public void onInventorySlotUpdate(int slot, ItemStack newStack) {
		setStackInSlot(slot, newStack);
	}
}
