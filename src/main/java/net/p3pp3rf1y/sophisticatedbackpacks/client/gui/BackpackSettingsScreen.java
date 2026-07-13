package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackSettingsTabControl;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainerMenu;
import net.p3pp3rf1y.sophisticatedcore.settings.StorageSettingsTabControlBase;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.IItemDisplaySettingsPreviewProvider;

public class BackpackSettingsScreen extends SettingsScreen {
	public BackpackSettingsScreen(SettingsContainerMenu<?> screenContainer, Inventory inv, Component titleIn) {
		super(screenContainer, inv, titleIn);
	}

	@Override
	protected StorageSettingsTabControlBase initializeTabControl() {
		return new BackpackSettingsTabControl(this, new Position(leftPos + imageWidth, topPos + 4));
	}

	@Override
	public IItemDisplaySettingsPreviewProvider getItemDisplaySettingsPreviewProvider() {
		return BackpackItemDisplaySettingsPreviewProvider.INSTANCE;
	}

	public static BackpackSettingsScreen constructScreen(SettingsContainerMenu<?> settingsContainer, Inventory playerInventory, Component title) {
		return new BackpackSettingsScreen(settingsContainer, playerInventory, title);
	}

	@Override
	protected void sendStorageInventoryScreenOpenMessage() {
		PacketDistributor.sendToServer(new BackpackOpenPayload());
	}
}
