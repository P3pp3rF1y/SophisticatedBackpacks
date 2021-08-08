package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.minecraftforge.energy.IEnergyStorage;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEnergyStorageUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IFluidHandlerWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeAccessModifier;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IUpgradeWrapperAccessor;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public class InceptionUpgradeWrapper extends UpgradeWrapperBase<InceptionUpgradeWrapper, InceptionUpgradeItem>
		implements IInventoryWrapperUpgrade, IUpgradeAccessModifier, IFluidHandlerWrapperUpgrade, IEnergyStorageUpgradeWrapper {
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
		}
		return inventory;

	}

	private void initSubBackpacksHandler() {
		subBackpacksHandler = new SubBackpacksHandler(backpackWrapper.getInventoryHandler());
	}

	@Override
	public IUpgradeWrapperAccessor wrapAccessor(IUpgradeWrapperAccessor upgradeWrapperAccessor) {
		if (Boolean.TRUE.equals(Config.COMMON.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get())) {
			initSubBackpacksHandler();
			return new InceptionWrapperAccessor(backpackWrapper, subBackpacksHandler);
		}
		return upgradeWrapperAccessor;
	}

	@Override
	@Nullable
	public IFluidHandlerItem wrapHandler(@Nullable IFluidHandlerItem fluidHandler, ItemStack backpack) {
		if (Boolean.TRUE.equals(Config.COMMON.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get())) {
			initSubBackpacksHandler();
			return new InceptionFluidHandler(fluidHandler, backpack, getInventoryOrder(), subBackpacksHandler);
		}
		return fluidHandler;
	}

	@Override
	@Nullable
	public IEnergyStorage wrapStorage(@Nullable IEnergyStorage energyStorage) {
		if (Boolean.TRUE.equals(Config.COMMON.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get())) {
			initSubBackpacksHandler();
			return new InceptionEnergyStorage(energyStorage, getInventoryOrder(), subBackpacksHandler);
		}
		return energyStorage;
	}
}
