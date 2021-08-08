package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.nbt.CompoundNBT;

public interface ISyncedContainer {
	void handleMessage(CompoundNBT data);
}
