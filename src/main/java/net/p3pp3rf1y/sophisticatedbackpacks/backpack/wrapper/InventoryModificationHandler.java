package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;

import java.util.List;

public class InventoryModificationHandler {
	private final ItemStack backpack;
	private final IBackpackWrapper backpackWrapper;
	private IItemHandlerModifiable modifiedInventoryHandler;

	public InventoryModificationHandler(ItemStack backpack, IBackpackWrapper backpackWrapper) {
		this.backpack = backpack;
		this.backpackWrapper = backpackWrapper;
	}

	public IItemHandlerModifiable getModifiedInventoryHandler() {
		if (modifiedInventoryHandler == null) {
			initializeInceptionInventoryHandler(backpackWrapper.getInventoryHandler());
		}
		return modifiedInventoryHandler;
	}

	private void initializeInceptionInventoryHandler(BackpackInventoryHandler inventoryHandler) {
		List<InceptionUpgradeItem.Wrapper> inceptionUpgrades = backpackWrapper.getUpgradeHandler().getTypeWrappers(InceptionUpgradeItem.TYPE);
		if (!inceptionUpgrades.isEmpty() && Boolean.TRUE.equals(Config.COMMON.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get())) {
			InceptionUpgradeItem.Wrapper firstUpgrade = inceptionUpgrades.get(0);
			setModifiedInventoryHandler(new InceptionInventoryHandler(backpack, inventoryHandler, firstUpgrade.getInventoryOrder()));
		} else {
			setModifiedInventoryHandler(inventoryHandler);
		}
	}

	private void setModifiedInventoryHandler(IItemHandlerModifiable inventoryAfterModifierUpgrades) {
		modifiedInventoryHandler = new InsertResponseInventoryWrapper(backpack, inventoryAfterModifierUpgrades);
	}
}
