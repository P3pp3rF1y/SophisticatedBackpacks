package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackSettingsTabControl;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.settings.StorageSettingsTabControl;

public class BackpackSettingsScreen extends SettingsScreen {
	public BackpackSettingsScreen(SettingsContainer screenContainer, Inventory inv, Component titleIn) {
		super(screenContainer, inv, titleIn);
	}

	@Override
	protected StorageSettingsTabControl initializeTabControl() {
		return new BackpackSettingsTabControl(this, new Position(leftPos + imageWidth, topPos + 4));
	}

	public static BackpackSettingsScreen constructScreen(SettingsContainer<?> settingsContainer, Inventory playerInventory, Component title) {
		return new BackpackSettingsScreen(settingsContainer, playerInventory, title);
	}

	@Override
	protected void sendStorageInventoryScreenOpenMessage() {
		SophisticatedBackpacks.PACKET_HANDLER.sendToServer(new BackpackOpenMessage());
	}
}
