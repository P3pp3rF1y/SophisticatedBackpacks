package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeAccessModifier;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IUpgradeWrapperAccessor;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class InceptionUpgradeWrapper extends UpgradeWrapperBase<InceptionUpgradeWrapper, InceptionUpgradeItem>
		implements IInventoryWrapperUpgrade, IUpgradeAccessModifier {
	private SubBackpacksHandler subBackpacksHandler = null;

	public InceptionUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
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
		backpackWrapper.refreshInventoryForUpgradeProcessing();
	}

	@Override
	public IItemHandlerModifiable wrapInventory(IItemHandlerModifiable inventory) {
		if (Boolean.TRUE.equals(Config.COMMON.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get())) {
			initSubBackpacksHandler();
			return new InceptionInventoryHandler(inventory, getInventoryOrder(), subBackpacksHandler);
		} else {
			return inventory;
		}
	}

	private void initSubBackpacksHandler() {
		subBackpacksHandler = new SubBackpacksHandler(backpackWrapper.getInventoryHandler());
	}

	@Override
	public IUpgradeWrapperAccessor wrapAccessor(IUpgradeWrapperAccessor upgradeWrapperAccessor) {
		if (Boolean.TRUE.equals(Config.COMMON.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get())) {
			initSubBackpacksHandler();
			return new InceptionWrapperAccessor(backpackWrapper, subBackpacksHandler);
		} else {
			return upgradeWrapperAccessor;
		}
	}
}
