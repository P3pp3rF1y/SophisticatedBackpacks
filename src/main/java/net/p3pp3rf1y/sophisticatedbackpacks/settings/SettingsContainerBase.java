package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncContainerClientDataMessage;

import java.util.function.Supplier;

public abstract class SettingsContainerBase<C extends ISettingsCategory> {
	private final SettingsContainer settingsContainer;
	private final String categoryName;
	private final C category;

	protected SettingsContainerBase(SettingsContainer settingsContainer, String categoryName, C category) {
		this.settingsContainer = settingsContainer;
		this.categoryName = categoryName;
		this.category = category;
	}

	protected C getCategory() {
		return category;
	}

	protected SettingsContainer getSettingsContainer() {
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
		if (isServer()) {
			return;
		}
		CompoundNBT data = supplyData.get();
		data.putString("categoryName", categoryName);
		PacketHandler.sendToServer(new SyncContainerClientDataMessage(data));
	}

	protected boolean isServer() {
		return !settingsContainer.getPlayer().level.isClientSide;
	}

	public abstract void handleMessage(CompoundNBT data);
}
