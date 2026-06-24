package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.neoforged.neoforge.transfer.item.ItemResource;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;

public class BackpackInventoryHandler extends InventoryHandler {
	public BackpackInventoryHandler(int numberOfInventorySlots, IStorageWrapper storageWrapper, ContainerContents containerContents, Runnable saveHandler,
			int slotLimit) {
		super(numberOfInventorySlots, storageWrapper, containerContents, saveHandler, slotLimit, Config.SERVER.stackUpgrade);
	}

	@Override
	protected boolean isAllowed(ItemResource resource) {
		return !Config.SERVER.disallowedItems.isItemDisallowed(resource.getItem())
				&& (!(resource.getItem() instanceof BackpackItem) || (hasInceptionUpgrade() && isBackpackWithoutInceptionUpgrade(resource)));
	}

	private boolean hasInceptionUpgrade() {
		return storageWrapper.getUpgradeHandler().hasUpgrade(InceptionUpgradeItem.TYPE);
	}

	private boolean isBackpackWithoutInceptionUpgrade(ItemResource resource) {
		return (resource.getItem() instanceof BackpackItem)
				&& !BackpackWrapper.fromStack(resource.toStack()).getUpgradeHandler().hasUpgrade(InceptionUpgradeItem.TYPE);
	}
}
