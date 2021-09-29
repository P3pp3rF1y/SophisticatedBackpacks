package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.nbt.CompoundTag;

public interface ISyncedContainer {
	void handleMessage(CompoundTag data);
}
