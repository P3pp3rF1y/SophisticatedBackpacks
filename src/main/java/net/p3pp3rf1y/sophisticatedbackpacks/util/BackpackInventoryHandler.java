package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import javax.annotation.Nonnull;
import java.util.function.Consumer;
import java.util.function.IntConsumer;

public class BackpackInventoryHandler extends ItemStackHandler {
	private static final String INVENTORY_TAG = "inventory";
	private final ItemStack backpack;
	private final Consumer<ItemStack> backpackSaveHandler;
	private IntConsumer contentsChangeHandler = slot -> {};
	private boolean isMainInventoryHandlerWrappedByInception = false;

	public BackpackInventoryHandler(ItemStack backpack, Consumer<ItemStack> backpackSaveHandler) {
		super(getNumberOfSlots(backpack));
		this.backpack = backpack;
		this.backpackSaveHandler = backpackSaveHandler;
		NBTHelper.getCompound(backpack, INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	public void setParentContentsChangeHandler(IntConsumer contentsChangeHandler) {
		this.contentsChangeHandler = contentsChangeHandler;
	}

	@Override
	public void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		saveInventory();
		contentsChangeHandler.accept(slot);
	}

	@Override
	public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
		return !(stack.getItem() instanceof BackpackItem) || backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
				.map(w -> w.getUpgradeHandler().hasInceptionUpgrade()).orElse(false);
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

	@Nonnull
	@Override
	public ItemStack insertItem(int slot, @Nonnull ItemStack stack, boolean simulate) {
		if (isMainInventoryHandlerWrappedByInception) {
			return super.insertItem(slot, stack, simulate);
		}

		ItemStack ret = InsertResponseHelper.runOnBeforeInsert(slot, stack, simulate, this, backpack);
		if (ret.isEmpty()) {
			return ret;
		}

		ret = super.insertItem(slot, ret, simulate);

		if (ret == stack) {
			return ret;
		}

		InsertResponseHelper.runOnAfterInsert(slot, simulate, this, backpack);

		return ret;
	}

	public void setMainInventoryHandlerWrappedByInception(boolean mainInventoryHandlerWrappedByInception) {
		isMainInventoryHandlerWrappedByInception = mainInventoryHandlerWrappedByInception;
	}
}
