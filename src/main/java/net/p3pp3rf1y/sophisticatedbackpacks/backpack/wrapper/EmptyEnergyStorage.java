package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraftforge.energy.IEnergyStorage;

public class EmptyEnergyStorage implements IEnergyStorage {
	public static final EmptyEnergyStorage INSTANCE = new EmptyEnergyStorage();
	private EmptyEnergyStorage() {}

	@Override
	public int receiveEnergy(int maxReceive, boolean simulate) {
		return 0;
	}

	@Override
	public int extractEnergy(int maxExtract, boolean simulate) {
		return 0;
	}

	@Override
	public int getEnergyStored() {
		return 0;
	}

	@Override
	public int getMaxEnergyStored() {
		return 0;
	}

	@Override
	public boolean canExtract() {
		return false;
	}

	@Override
	public boolean canReceive() {
		return false;
	}
}
