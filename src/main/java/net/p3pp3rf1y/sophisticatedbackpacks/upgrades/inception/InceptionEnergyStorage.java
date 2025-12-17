package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.neoforged.neoforge.transfer.energy.EnergyHandler;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class InceptionEnergyStorage implements EnergyHandler {
	@Nullable
	private final EnergyHandler wrappedEnergyStorage;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;

	private EnergyHandler[] energyHandlers;

	public InceptionEnergyStorage(@Nullable EnergyHandler wrappedEnergyStorage, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
		this.wrappedEnergyStorage = wrappedEnergyStorage;
		this.inventoryOrder = inventoryOrder;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(sbs -> refreshHandlers());
		refreshHandlers();
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
	}

	@Override
	public int insert(int amount, TransactionContext tx) {
		int totalInserted = 0;
		for (EnergyHandler storage : energyHandlers) {
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
		for (EnergyHandler storage : energyHandlers) {
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
		for (EnergyHandler storage : energyHandlers) {
			totalEnergyStored += storage.getAmountAsLong();
		}
		return totalEnergyStored;
	}

	@Override
	public long getCapacityAsLong() {
		long totalMaxEnergy = 0;
		for (EnergyHandler storage : energyHandlers) {
			totalMaxEnergy += storage.getCapacityAsLong();
		}
		return totalMaxEnergy;
	}
}
