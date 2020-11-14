package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class BackpackUpgradeHandler extends ItemStackHandler {
	private static final String UPGRADE_INVENTORY_TAG = "upgradeInventory";
	private final ItemStack backpack;
	private final Consumer<ItemStack> backpackSaveHandler;

	public BackpackUpgradeHandler(ItemStack backpack, Consumer<ItemStack> backpackSaveHandler) {
		super(getNumberOfUpgradeSlots(backpack));
		this.backpack = backpack;
		this.backpackSaveHandler = backpackSaveHandler;
		NBTHelper.getCompound(backpack, UPGRADE_INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	@Override
	protected void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		backpack.setTagInfo(UPGRADE_INVENTORY_TAG, serializeNBT());
		backpackSaveHandler.accept(backpack);
	}

	@Override
	public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
		return stack.getItem() instanceof IBackpackUpgrade;
	}

	private static int getNumberOfUpgradeSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfUpgradeSlots();
	}

	public void copyTo(BackpackUpgradeHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

	public List<ItemStack> getUpgradeStacks(Predicate<ItemStack> matches) {
		List<ItemStack> ret = new ArrayList<>();
		for (int slot = 0; slot < getSlots(); slot++) {
			ItemStack slotStack = getStackInSlot(slot);
			if (!slotStack.isEmpty() && matches.test(slotStack)) {
				ret.add(slotStack);
			}
		}
		return ret;
	}
}

