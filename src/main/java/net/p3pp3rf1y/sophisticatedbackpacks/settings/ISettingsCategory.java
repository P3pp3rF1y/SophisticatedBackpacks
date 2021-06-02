package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.nbt.CompoundNBT;

public interface ISettingsCategory {
	void reloadFrom(CompoundNBT categoryNbt);
}
