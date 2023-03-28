package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.minecraftforge.energy.IEnergyStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEnergyStorageUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IFluidHandlerWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeAccessModifier;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapperAccessor;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public class InceptionUpgradeWrapper extends UpgradeWrapperBase<InceptionUpgradeWrapper, InceptionUpgradeItem>
		implements IInventoryWrapperUpgrade, IUpgradeAccessModifier, IFluidHandlerWrapperUpgrade, IEnergyStorageUpgradeWrapper {
	private SubBackpacksHandler subBackpacksHandler = null;

	public InceptionUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
	}

	@Override
	public boolean hideSettingsTab() {
		return Boolean.FALSE.equals(Config.SERVER.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get());
	}

	public InventoryOrder getInventoryOrder() {
		return NBTHelper.getEnumConstant(upgrade, "inventoryOrder", InventoryOrder::fromName).orElse(InventoryOrder.MAIN_FIRST);
	}

	public void setInventoryOrder(InventoryOrder inventoryOrder) {
		NBTHelper.setEnumConstant(upgrade, "inventoryOrder", inventoryOrder);
		save();
		storageWrapper.refreshInventoryForUpgradeProcessing();
	}

	@Override
	public ITrackedContentsItemHandler wrapInventory(ITrackedContentsItemHandler inventory) {
		if (Boolean.TRUE.equals(Config.SERVER.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get())) {
			initSubBackpacksHandler();
			return new InceptionInventoryHandler(inventory, getInventoryOrder(), subBackpacksHandler);
		}
		return inventory;

	}

	private void initSubBackpacksHandler() {
		subBackpacksHandler = new SubBackpacksHandler(storageWrapper.getInventoryHandler());
	}

	@Override
	public IUpgradeWrapperAccessor wrapAccessor(IUpgradeWrapperAccessor upgradeWrapperAccessor) {
		if (Boolean.TRUE.equals(Config.SERVER.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get())) {
			initSubBackpacksHandler();
			return new InceptionWrapperAccessor(storageWrapper, subBackpacksHandler);
		}
		return upgradeWrapperAccessor;
	}

	@Override
	@Nullable
	public IStorageFluidHandler wrapHandler(@Nullable IStorageFluidHandler fluidHandler, ItemStack backpack) {
		if (Boolean.TRUE.equals(Config.SERVER.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get())) {
			initSubBackpacksHandler();
			return new InceptionFluidHandler(fluidHandler, backpack, getInventoryOrder(), subBackpacksHandler);
		}
		return fluidHandler;
	}

	@Override
	@Nullable
	public IEnergyStorage wrapStorage(@Nullable IEnergyStorage energyStorage) {
		if (Boolean.TRUE.equals(Config.SERVER.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get())) {
			initSubBackpacksHandler();
			return new InceptionEnergyStorage(energyStorage, getInventoryOrder(), subBackpacksHandler);
		}
		return energyStorage;
	}
}
