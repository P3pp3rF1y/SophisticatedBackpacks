package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.neoforged.neoforge.energy.IEnergyStorage;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;

import javax.annotation.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

public class InceptionEnergyStorage implements IEnergyStorage, InceptionHandler {
	@Nullable
	private final IEnergyStorage wrappedEnergyStorage;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;

	private IEnergyStorage[] energyStorages;
	private boolean handlersInvalidated;
	private final Consumer<Collection<IStorageWrapper>> refreshListener = subBackpacks -> refreshHandlers();
	private final Runnable upgradeCachesInvalidatedListener = () -> handlersInvalidated = true;

	public InceptionEnergyStorage(@Nullable IEnergyStorage wrappedEnergyStorage, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
		this.wrappedEnergyStorage = wrappedEnergyStorage;
		this.inventoryOrder = inventoryOrder;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(refreshListener);
		subBackpacksHandler.addUpgradeCachesInvalidatedListener(upgradeCachesInvalidatedListener);
		refreshHandlers();
	}

	@Override
	public void onBeforeDeconstruct() {
		subBackpacksHandler.removeRefreshListener(refreshListener);
		subBackpacksHandler.removeUpgradeCachesInvalidatedListener(upgradeCachesInvalidatedListener);
	}

	private void refreshHandlers() {
		List<IEnergyStorage> storages = new ArrayList<>();
		if (wrappedEnergyStorage != null && inventoryOrder == InventoryOrder.MAIN_FIRST) {
			storages.add(wrappedEnergyStorage);
		}
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> sbp.getEnergyStorage().ifPresent(storages::add));
		if (wrappedEnergyStorage != null && inventoryOrder == InventoryOrder.INCEPTED_FIRST) {
			storages.add(wrappedEnergyStorage);
		}
		energyStorages = storages.toArray(new IEnergyStorage[]{});
		handlersInvalidated = false;
	}

	private IEnergyStorage[] getEnergyStorages() {
		subBackpacksHandler.refresh();
		if (handlersInvalidated) {
			refreshHandlers();
		}
		return energyStorages;
	}

	@Override
	public int receiveEnergy(int maxReceive, boolean simulate) {
		int totalReceived = 0;
		for (IEnergyStorage storage : getEnergyStorages()) {
			totalReceived += storage.receiveEnergy(maxReceive - totalReceived, simulate);
			if (totalReceived == maxReceive) {
				break;
			}
		}

		return totalReceived;
	}

	@Override
	public int extractEnergy(int maxExtract, boolean simulate) {
		int totalExtracted = 0;
		for (IEnergyStorage storage : getEnergyStorages()) {
			totalExtracted += storage.extractEnergy(maxExtract - totalExtracted, simulate);
			if (totalExtracted == maxExtract) {
				break;
			}
		}

		return totalExtracted;
	}

	@Override
	public int getEnergyStored() {
		int totalEnergyStored = 0;
		for (IEnergyStorage storage : getEnergyStorages()) {
			totalEnergyStored += storage.getEnergyStored();
		}
		return totalEnergyStored;
	}

	@Override
	public int getMaxEnergyStored() {
		int totalMaxEnergy = 0;

		for (IEnergyStorage storage : getEnergyStorages()) {
			if (totalMaxEnergy > Integer.MAX_VALUE - storage.getMaxEnergyStored()) {
				return Integer.MAX_VALUE;
			}

			totalMaxEnergy += storage.getMaxEnergyStored();
		}

		return totalMaxEnergy;
	}

	@Override
	public boolean canExtract() {
		return getEnergyStorages().length > 0;
	}

	@Override
	public boolean canReceive() {
		return getEnergyStorages().length > 0;
	}
}
