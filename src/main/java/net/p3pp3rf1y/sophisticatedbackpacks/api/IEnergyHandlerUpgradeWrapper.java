package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.neoforged.neoforge.transfer.energy.EnergyHandler;

import javax.annotation.Nullable;

public interface IEnergyHandlerUpgradeWrapper {
	@Nullable
	EnergyHandler wrapStorage(@Nullable EnergyHandler energyStorage);
}
