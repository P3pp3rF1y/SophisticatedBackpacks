package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncContainerClientDataMessage;

import java.util.function.Supplier;

public abstract class SettingsContainerBase<C extends ISettingsCategory> {
	private final SlotSettingsContainer settingsContainer;
	private final String categoryName;
	private final C category;

	protected SettingsContainerBase(SlotSettingsContainer settingsContainer, String categoryName, C category) {
		this.settingsContainer = settingsContainer;
		this.categoryName = categoryName;
		this.category = category;
	}

	protected C getCategory() {
		return category;
	}

	protected SlotSettingsContainer getSettingsContainer() {
		return settingsContainer;
	}

	public void sendIntToServer(String key, int value) {
		sendDataToServer(() -> {
			CompoundNBT data = new CompoundNBT();
			data.putInt(key, value);
			return data;
		});
	}

	public void sendStringToServer(String key, String value) {
		sendDataToServer(() -> {
			CompoundNBT data = new CompoundNBT();
			data.putString(key, value);
			return data;
		});
	}

	public void sendDataToServer(Supplier<CompoundNBT> supplyData) {
		if (!settingsContainer.getPlayer().world.isRemote) {
			return;
		}
		CompoundNBT data = supplyData.get();
		data.putString("categoryName", categoryName);
		PacketHandler.sendToServer(new SyncContainerClientDataMessage(data));
	}

	public abstract void handleMessage(CompoundNBT data);
}
