package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackContentsMessage;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.SETTINGS_CONTAINER_TYPE;

public class BackpackSettingsContainer extends SettingsContainer<IBackpackWrapper> implements IContextAwareContainer {
	private final BackpackContext backpackContext;
	private CompoundTag lastSettingsNbt = null;

	protected BackpackSettingsContainer(int windowId, Player player, BackpackContext backpackContext) {
		super(SETTINGS_CONTAINER_TYPE.get(), windowId, player, backpackContext.getBackpackWrapper(player));

		this.backpackContext = backpackContext;
	}

	public static BackpackSettingsContainer fromBuffer(int windowId, Inventory playerInventory, FriendlyByteBuf packetBuffer) {
		return new BackpackSettingsContainer(windowId, playerInventory.player, BackpackContext.fromBuffer(packetBuffer));
	}

	@Override
	public void detectSettingsChangeAndReload() {
		if (player.level.isClientSide) {
			storageWrapper.getContentsUuid().ifPresent(uuid -> {
				BackpackStorage storage = BackpackStorage.get();
				if (storage.removeUpdatedBackpackSettingsFlag(uuid)) {
					storageWrapper.getSettingsHandler().reloadFrom(storage.getOrCreateBackpackContents(uuid));
				}
			});
		}
	}

	@Override
	public void broadcastChanges() {
		super.broadcastChanges();

		sendBackpackSettingsToClient();
	}

	private void sendBackpackSettingsToClient() {
		if (player.level.isClientSide) {
			return;
		}

		if (lastSettingsNbt == null || !lastSettingsNbt.equals(storageWrapper.getSettingsHandler().getNbt())) {
			lastSettingsNbt = storageWrapper.getSettingsHandler().getNbt().copy();
		}

		storageWrapper.getContentsUuid().ifPresent(uuid -> {
			CompoundTag settingsContents = new CompoundTag();
			CompoundTag settingsNbt = storageWrapper.getSettingsHandler().getNbt();
			if (!settingsNbt.isEmpty()) {
				settingsContents.put(SettingsHandler.SETTINGS_TAG, settingsNbt);
				SophisticatedBackpacks.PACKET_HANDLER.sendToClient((ServerPlayer) player, new BackpackContentsMessage(uuid, settingsContents));
			}
		});
	}

	@Override
	public BackpackContext getBackpackContext() {
		return backpackContext;
	}
}
