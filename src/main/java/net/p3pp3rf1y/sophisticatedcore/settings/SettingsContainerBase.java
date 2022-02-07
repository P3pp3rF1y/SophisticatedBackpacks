package net.p3pp3rf1y.sophisticatedcore.settings;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.network.SyncContainerClientDataMessage;

import java.util.function.Supplier;

public abstract class SettingsContainerBase<C extends ISettingsCategory> {
	private final SettingsContainer<?> settingsContainer;
	private final String categoryName;
	private final C category;

	protected SettingsContainerBase(SettingsContainer<?> settingsContainer, String categoryName, C category) {
		this.settingsContainer = settingsContainer;
		this.categoryName = categoryName;
		this.category = category;
	}

	protected C getCategory() {
		return category;
	}

	protected SettingsContainer<?> getSettingsContainer() {
		return settingsContainer;
	}

	public void sendIntToServer(String key, int value) {
		sendDataToServer(() -> {
			CompoundTag data = new CompoundTag();
			data.putInt(key, value);
			return data;
		});
	}

	public void sendStringToServer(String key, String value) {
		sendDataToServer(() -> {
			CompoundTag data = new CompoundTag();
			data.putString(key, value);
			return data;
		});
	}

	public void sendDataToServer(Supplier<CompoundTag> supplyData) {
		if (isServer()) {
			return;
		}
		CompoundTag data = supplyData.get();
		data.putString("categoryName", categoryName);
		SophisticatedCore.PACKET_HANDLER.sendToServer(new SyncContainerClientDataMessage(data));
	}

	protected boolean isServer() {
		return !settingsContainer.getPlayer().level.isClientSide;
	}

	public abstract void handleMessage(CompoundTag data);
}
