package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;

public class BackpackInventoryHandler extends InventoryHandler {
	public BackpackInventoryHandler(int numberOfInventorySlots, IStorageWrapper storageWrapper, CompoundTag contentsNbt, Runnable saveHandler, int slotLimit) {
		super(numberOfInventorySlots, storageWrapper, contentsNbt, saveHandler, slotLimit, Config.SERVER.stackUpgrade);
	}

	@Override
	protected boolean isAllowed(ItemStack stack) {
		return !Config.SERVER.disallowedItems.isItemDisallowed(stack.getItem())
				&& (!(stack.getItem() instanceof BackpackItem) || (hasInceptionUpgrade() && isBackpackWithoutInceptionUpgrade(stack)));
	}

	private boolean hasInceptionUpgrade() {
		return storageWrapper.getUpgradeHandler().hasUpgrade(InceptionUpgradeItem.TYPE);
	}

	private boolean isBackpackWithoutInceptionUpgrade(ItemStack stack) {
		return stack.getItem() instanceof BackpackItem && !hasInstalledInception(new BackpackWrapper(stack))
				&& !hasInstalledInception(BackpackWrapper.fromStack(stack));
	}

	private boolean hasInstalledInception(IStorageWrapper wrapper) {
		for (int slot = 0; slot < wrapper.getUpgradeHandler().getSlots(); slot++) {
			if (wrapper.getUpgradeHandler().getStackInSlot(slot).getItem() instanceof InceptionUpgradeItem) {
				return true;
			}
		}
		return false;
	}
}
