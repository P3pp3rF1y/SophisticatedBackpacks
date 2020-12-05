package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;

import javax.annotation.Nonnull;
import java.util.List;
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

	@Override
	public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
		return !(stack.getItem() instanceof BackpackItem) || hasInceptionUpgrade();
	}

	private boolean hasInceptionUpgrade() {
		return backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
				.map(w -> !w.getUpgradeHandler().getTypeWrappers(InceptionUpgradeItem.TYPE).isEmpty()).orElse(false);
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
		ItemStack ret = backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
				.map(wrapper -> {
					List<IInsertResponseUpgrade> wrappers = wrapper.getUpgradeHandler().getWrappersThatImplement(IInsertResponseUpgrade.class);
					ItemStack remaining = stack;
					for (IInsertResponseUpgrade upgrade : wrappers) {
						remaining = upgrade.onBeforeInsert(this, slot, remaining, simulate);
						if (remaining.isEmpty()) {
							return ItemStack.EMPTY;
						}
					}
					return remaining;
				}).orElse(stack);
		if (ret.isEmpty()) {
			return ret;
		}

		ret = super.insertItem(slot, ret, simulate);

		if (ret == stack) {
			return ret;
		}

		if (!simulate) {
			backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
					.ifPresent(wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(IInsertResponseUpgrade.class)
							.forEach(u -> u.onAfterInsert(this, slot)));
		}

		return ret;
	}
}
