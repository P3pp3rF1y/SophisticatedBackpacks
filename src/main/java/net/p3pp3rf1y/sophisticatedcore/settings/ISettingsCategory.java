package net.p3pp3rf1y.sophisticatedcore.settings;

import net.minecraft.nbt.CompoundTag;

public interface ISettingsCategory {
	void reloadFrom(CompoundTag categoryNbt);
}
