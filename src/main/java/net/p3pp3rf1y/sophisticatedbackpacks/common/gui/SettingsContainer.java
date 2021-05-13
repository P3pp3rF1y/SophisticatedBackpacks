package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncContainerClientDataMessage;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.SETTINGS_CONTAINER_TYPE;

public class SettingsContainer extends Container implements ISyncedContainer {
	private final PlayerEntity player;
	private final BackpackContext backpackContext;

	protected SettingsContainer(int windowId, PlayerEntity player, BackpackContext backpackContext) {
		super(SETTINGS_CONTAINER_TYPE.get(), windowId);
		this.player = player;
		this.backpackContext = backpackContext;
	}

	@Override
	public boolean canInteractWith(PlayerEntity player) {
		return true;
	}

	public static SettingsContainer fromBuffer(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new SettingsContainer(windowId, playerInventory.player, BackpackContext.fromBuffer(packetBuffer));
	}

	public BackpackContext getBackpackContext() {
		return backpackContext;
	}

	public <T> T getBackpackSetting(boolean isPlayerLevelSetting, BackpackSettingsManager.BackpackSetting<T> setting) {
		if (isPlayerLevelSetting) {
			return BackpackSettingsManager.getPlayerSettingOrDefault(player, setting);
		}
		return BackpackSettingsManager.getBackpackSettingValue(player, backpackContext.getBackpackWrapper(player).getBackpack(), setting);
	}

	public <T> void setBackpackSetting(boolean isPlayerLevelSetting, BackpackSettingsManager.BackpackSetting<T> setting, T value) {
		if (player.world.isRemote) {
			CompoundNBT data = new CompoundNBT();
			data.putBoolean("isPlayerLevelSetting", isPlayerLevelSetting);
			data.putString("settingName", setting.getName());
			CompoundNBT settingValue = new CompoundNBT();
			setting.setValue(settingValue, value);
			data.put("settingValue", settingValue);
			PacketHandler.sendToServer(new SyncContainerClientDataMessage(data));
		}
		if (isPlayerLevelSetting) {
			BackpackSettingsManager.setPlayerSetting(player, setting, value);
		} else {
			BackpackSettingsManager.setItemSetting(player, backpackContext.getBackpackWrapper(player).getBackpack(), setting, value);
		}
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		boolean isPlayerLevelSetting = data.getBoolean("isPlayerLevelSetting");
		String settingName = data.getString("settingName");
		CompoundNBT settingValue = data.getCompound("settingValue");

		BackpackSettingsManager.BackpackSetting<?> setting = BackpackSettingsManager.getBackpackSetting(settingName);
		setBackpackSetting(isPlayerLevelSetting, settingValue, setting);
	}

	private <T> void setBackpackSetting(boolean isPlayerLevelSetting, CompoundNBT settingValue, BackpackSettingsManager.BackpackSetting<T> setting) {
		setBackpackSetting(isPlayerLevelSetting, setting, setting.getValue(settingValue).orElse(setting.getDefaultValue()));
	}
}
