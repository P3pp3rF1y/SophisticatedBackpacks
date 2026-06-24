package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.transfer.energy.EnergyHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEnergyHandlerUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IFluidHandlerWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInventoryWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemResourceHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeAccessModifier;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapperAccessor;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import org.jspecify.annotations.Nullable;

import java.util.function.Consumer;

public class InceptionUpgradeWrapper extends UpgradeWrapperBase<InceptionUpgradeWrapper, InceptionUpgradeItem>
		implements
			IInventoryWrapperUpgrade,
			IUpgradeAccessModifier,
			IFluidHandlerWrapperUpgrade,
			IEnergyHandlerUpgradeWrapper {
	private SubBackpacksHandler subBackpacksHandler = null;

	public InceptionUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
	}

	@Override
	public boolean hideSettingsTab() {
		return !Config.SERVER.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get();
	}

	public InventoryOrder getInventoryOrder() {
		return upgrade.getOrDefault(ModDataComponents.INVENTORY_ORDER, InventoryOrder.MAIN_FIRST);
	}

	public void setInventoryOrder(InventoryOrder inventoryOrder) {
		upgrade.set(ModDataComponents.INVENTORY_ORDER, inventoryOrder);
		save();
		storageWrapper.refreshInventoryForUpgradeProcessing();
	}

	@Override
	public ITrackedContentsItemResourceHandler wrapInventory(ITrackedContentsItemResourceHandler inventory) {
		if (Config.SERVER.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get()) {
			initSubBackpacksHandler();
			return new InceptionInventoryHandler(inventory, getInventoryOrder(), subBackpacksHandler);
		}
		return inventory;

	}

	private void initSubBackpacksHandler() {
		boolean cacheSubBackpackWrappers = !(storageWrapper instanceof BackpackWrapper backpackWrapper)
				|| backpackWrapper.shouldCacheContainedBackpackWrappers();
		subBackpacksHandler = new SubBackpacksHandler(storageWrapper.getInventoryHandler(), cacheSubBackpackWrappers);
	}

	@Override
	public IUpgradeWrapperAccessor wrapAccessor(IUpgradeWrapperAccessor upgradeWrapperAccessor) {
		if (Config.SERVER.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get()) {
			initSubBackpacksHandler();
			return new InceptionWrapperAccessor(storageWrapper, subBackpacksHandler);
		}
		return upgradeWrapperAccessor;
	}

	@Override
	@Nullable
	public IStorageFluidHandler wrapHandler(@Nullable IStorageFluidHandler fluidHandler, ItemStack backpack) {
		if (Config.SERVER.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get()) {
			initSubBackpacksHandler();
			return new InceptionFluidHandler(fluidHandler, getInventoryOrder(), subBackpacksHandler);
		}
		return fluidHandler;
	}

	@Override
	@Nullable
	public EnergyHandler wrapStorage(@Nullable EnergyHandler energyStorage) {
		if (Config.SERVER.inceptionUpgrade.upgradesInContainedBackpacksAreFunctional.get()) {
			initSubBackpacksHandler();
			return new InceptionEnergyStorage(energyStorage, getInventoryOrder(), subBackpacksHandler);
		}
		return energyStorage;
	}
}
