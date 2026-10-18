package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.neoforged.neoforge.transfer.energy.EnergyHandler;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

public class InceptionEnergyStorage implements EnergyHandler, InceptionHandler {
	@Nullable
	private final EnergyHandler wrappedEnergyStorage;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;

	private EnergyHandler[] energyHandlers;
	private boolean handlersInvalidated;
	private final Consumer<Collection<IStorageWrapper>> refreshListener = subBackpacks -> refreshHandlers();
	private final Runnable upgradeCachesInvalidatedListener = () -> handlersInvalidated = true;

	public InceptionEnergyStorage(@Nullable EnergyHandler wrappedEnergyStorage, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
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
		List<EnergyHandler> storages = new ArrayList<>();
		if (wrappedEnergyStorage != null && inventoryOrder == InventoryOrder.MAIN_FIRST) {
			storages.add(wrappedEnergyStorage);
		}
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> sbp.getEnergyHandler().ifPresent(storages::add));
		if (wrappedEnergyStorage != null && inventoryOrder == InventoryOrder.INCEPTED_FIRST) {
			storages.add(wrappedEnergyStorage);
		}
		energyHandlers = storages.toArray(new EnergyHandler[]{});
		handlersInvalidated = false;
	}

	private EnergyHandler[] getEnergyHandlers() {
		if (handlersInvalidated) {
			refreshHandlers();
		}
		return energyHandlers;
	}

	@Override
	public int insert(int amount, TransactionContext tx) {
		int totalInserted = 0;
		for (EnergyHandler storage : getEnergyHandlers()) {
			totalInserted += storage.insert(amount - totalInserted, tx);
			if (totalInserted == amount) {
				break;
			}
		}

		return totalInserted;
	}

	@Override
	public int extract(int amount, TransactionContext tx) {
		int totalExtracted = 0;
		for (EnergyHandler storage : getEnergyHandlers()) {
			totalExtracted += storage.extract(amount - totalExtracted, tx);
			if (totalExtracted == amount) {
				break;
			}
		}
		return totalExtracted;
	}

	@Override
	public long getAmountAsLong() {
		long totalEnergyStored = 0;
		for (EnergyHandler storage : getEnergyHandlers()) {
			totalEnergyStored += storage.getAmountAsLong();
		}
		return totalEnergyStored;
	}

	@Override
	public long getCapacityAsLong() {
		long totalMaxEnergy = 0;
		for (EnergyHandler storage : getEnergyHandlers()) {
			totalMaxEnergy += storage.getCapacityAsLong();
		}
		return totalMaxEnergy;
	}
}
