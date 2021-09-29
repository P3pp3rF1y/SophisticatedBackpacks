package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.nbt.CompoundTag;

public interface ISettingsCategory {
	void reloadFrom(CompoundTag categoryNbt);
}
