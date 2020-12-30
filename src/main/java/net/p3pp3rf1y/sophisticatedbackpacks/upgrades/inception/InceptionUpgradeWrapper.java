package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IObservableItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class InceptionUpgradeWrapper extends UpgradeWrapperBase<InceptionUpgradeWrapper, InceptionUpgradeItem> implements IInventoryWrapperUpgrade {
	public InceptionUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
	}

	@Override
	public boolean hideSettingsTab() {
		return Boolean.FALSE.equals(Config.COMMON.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get());
	}

	public InventoryOrder getInventoryOrder() {
		return NBTHelper.getEnumConstant(upgrade, "inventoryOrder", InventoryOrder::fromName).orElse(InventoryOrder.MAIN_FIRST);
	}

	public void setInventoryOrder(InventoryOrder inventoryOrder) {
		NBTHelper.setEnumConstant(upgrade, "inventoryOrder", inventoryOrder);
		save();
	}

	@Override
	public void onNbtChange(IBackpackWrapper backpackWrapper) {
		backpackWrapper.refreshInventoryForUpgradeProcessing();
	}

	@Override
	public IObservableItemHandler wrapInventory(ItemStack backpack, IObservableItemHandler inventory) {
		if (Boolean.TRUE.equals(Config.COMMON.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get())) {
			return new InceptionInventoryHandler(backpack, inventory, getInventoryOrder());
		} else {
			return inventory;
		}
	}
}
