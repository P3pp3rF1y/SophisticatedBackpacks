package net.p3pp3rf1y.sophisticatedcore.common.gui;

import net.minecraft.nbt.CompoundTag;

import java.util.function.Supplier;

public interface IServerUpdater {
	void sendBooleanToServer(String key, boolean value);

	void sendDataToServer(Supplier<CompoundTag> supplyData);
}
