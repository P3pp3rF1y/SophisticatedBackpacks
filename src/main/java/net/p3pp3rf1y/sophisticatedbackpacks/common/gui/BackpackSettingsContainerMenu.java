package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackContentsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainerMenu;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.SETTINGS_CONTAINER_TYPE;

public class BackpackSettingsContainerMenu extends SettingsContainerMenu<IBackpackWrapper> implements IContextAwareContainer {
	static {
		SettingsContainerMenu.addFactory(BackpackMainSettingsCategory.NAME, BackpackMainSettingsContainer::new);
	}

	private final BackpackContext backpackContext;
	private CompoundTag lastSettingsNbt = null;

	protected BackpackSettingsContainerMenu(int windowId, Player player, BackpackContext backpackContext) {
		super(SETTINGS_CONTAINER_TYPE.get(), windowId, player, backpackContext.getBackpackWrapper(player));

		this.backpackContext = backpackContext;
	}

	public static BackpackSettingsContainerMenu fromBuffer(int windowId, Inventory playerInventory, FriendlyByteBuf packetBuffer) {
		return new BackpackSettingsContainerMenu(windowId, playerInventory.player, BackpackContext.fromBuffer(packetBuffer, playerInventory.player.level));
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

			storageWrapper.getContentsUuid().ifPresent(uuid -> {
				CompoundTag settingsContents = new CompoundTag();
				CompoundTag settingsNbt = storageWrapper.getSettingsHandler().getNbt();
				if (!settingsNbt.isEmpty()) {
					settingsContents.put(BackpackSettingsHandler.SETTINGS_TAG, settingsNbt);
					SBPPacketHandler.INSTANCE.sendToClient((ServerPlayer) player, new BackpackContentsMessage(uuid, settingsContents));
				}
			});
		}
	}

	@Override
	public BackpackContext getBackpackContext() {
		return backpackContext;
	}
}
