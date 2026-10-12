package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;

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
		return hasInstalledInception(storageWrapper);
	}

	private boolean isBackpackWithoutInceptionUpgrade(ItemStack stack) {
		LinkedStorageEndpointData child = LinkedStorageStackData.getEndpoint(stack);
		if (child != null && storageWrapper.getContentsUuid().filter(child.groupId()::equals).isPresent()) {
			return false;
		}
		return stack.getItem() instanceof BackpackItem && !hasInstalledInception(new BackpackWrapper(stack))
				&& !stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(this::hasInstalledInception).orElse(false);
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
