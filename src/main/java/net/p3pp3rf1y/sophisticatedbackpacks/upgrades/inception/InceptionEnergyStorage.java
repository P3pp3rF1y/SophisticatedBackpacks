package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraftforge.energy.IEnergyStorage;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

public class InceptionEnergyStorage implements IEnergyStorage {
	@Nullable
	private final IEnergyStorage wrappedEnergyStorage;
	private final InventoryOrder inventoryOrder;
	private final SubBackpacksHandler subBackpacksHandler;

	private IEnergyStorage[] energyStorages;

	public InceptionEnergyStorage(@Nullable IEnergyStorage wrappedEnergyStorage, InventoryOrder inventoryOrder, SubBackpacksHandler subBackpacksHandler) {
		this.wrappedEnergyStorage = wrappedEnergyStorage;
		this.inventoryOrder = inventoryOrder;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(sbs -> refreshHandlers());
		refreshHandlers();
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
		energyStorages = storages.toArray(new IEnergyStorage[] {});
	}

	@Override
	public int receiveEnergy(int maxReceive, boolean simulate) {
		int totalReceived = 0;
		for (IEnergyStorage storage : energyStorages) {
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
		for (IEnergyStorage storage : energyStorages) {
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
		for (IEnergyStorage storage : energyStorages) {
			totalEnergyStored += storage.getEnergyStored();
		}
		return totalEnergyStored;
	}

	@Override
	public int getMaxEnergyStored() {
		int totalMaxEnergy = 0;

		for (IEnergyStorage storage : energyStorages) {
			if (totalMaxEnergy > Integer.MAX_VALUE - storage.getMaxEnergyStored()) {
				return Integer.MAX_VALUE;
			}

			totalMaxEnergy += storage.getMaxEnergyStored();
		}

		return totalMaxEnergy;
	}

	@Override
	public boolean canExtract() {
		return energyStorages.length > 0;
	}

	@Override
	public boolean canReceive() {
		return energyStorages.length > 0;
	}
}
