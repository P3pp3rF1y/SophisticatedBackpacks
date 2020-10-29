package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import javax.annotation.Nonnull;
import java.util.Optional;
import java.util.function.Predicate;

public class BackpackUpgradeHandler extends ItemStackHandler {
	private static final String UPGRADE_INVENTORY_TAG = "upgradeInventory";
	private final ItemStack backpack;

	public BackpackUpgradeHandler(ItemStack backpack) {
		super(getNumberOfUpgradeSlots(backpack));
		this.backpack = backpack;
		NBTHelper.getCompound(backpack, UPGRADE_INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	@Override
	protected void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		backpack.setTagInfo(UPGRADE_INVENTORY_TAG, serializeNBT());
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

	public Optional<ItemStack> getUpgrade(Predicate<ItemStack> matches) {
		for (int slot = 0; slot < getSlots(); slot++) {
			ItemStack slotStack = getStackInSlot(slot);
			if (!slotStack.isEmpty() && matches.test(slotStack)) {
				return Optional.of(slotStack);
			}
		}
		return Optional.empty();
	}
}

