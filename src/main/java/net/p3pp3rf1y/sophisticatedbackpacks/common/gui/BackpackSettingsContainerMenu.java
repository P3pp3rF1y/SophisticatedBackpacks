package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.network.LinkedStorageBackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainerMenu;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;

import java.util.Optional;
import java.util.UUID;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.SETTINGS_CONTAINER_TYPE;

public class BackpackSettingsContainerMenu extends SettingsContainerMenu<IBackpackWrapper> implements IContextAwareContainer {
	static {
		addFactory(BackpackMainSettingsCategory.NAME, BackpackMainSettingsContainer::new);
	}

	private final BackpackContext backpackContext;
	private CompoundTag lastSettingsNbt = null;

	protected BackpackSettingsContainerMenu(int windowId, Player player, BackpackContext backpackContext) {
		super(SETTINGS_CONTAINER_TYPE.get(), windowId, player, backpackContext.getBackpackWrapper(player));

		this.backpackContext = backpackContext;
		if (!player.level().isClientSide() && (backpackContext.getType() == BackpackContext.ContextType.ITEM_BACKPACK
				|| backpackContext.getType() == BackpackContext.ContextType.ITEM_SUB_BACKPACK)) {
			storageWrapper.onInit(player.level());
		}
	}

	public static BackpackSettingsContainerMenu fromBuffer(int windowId, Inventory playerInventory, FriendlyByteBuf buffer) {
		return new BackpackSettingsContainerMenu(windowId, playerInventory.player, BackpackContext.fromBuffer(buffer, playerInventory.player.level()));
	}

	@Override
	public void detectSettingsChangeAndReload() {
		if (player.level().isClientSide) {
			Optional<UUID> linkedStorageGroupId = getLinkedStorageGroupId();
			if (linkedStorageGroupId.isPresent() && ClientLinkedStorageBackpackContents.removeUpdatedGroup(linkedStorageGroupId.get())) {
				ILinkedStorageContentsBinding contents = ClientLinkedStorageBackpackContents.getBinding(linkedStorageGroupId.get())
						.orElseThrow(() -> new IllegalStateException("Updated linked backpack group has no snapshot: " + linkedStorageGroupId.get()));
				storageWrapper.getSettingsHandler().reloadFrom(contents.getContents());
				return;
			}
			if (linkedStorageGroupId.isPresent()) {
				return;
			}
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
		if (player.level().isClientSide) {
			return;
		}

		if (lastSettingsNbt == null || !lastSettingsNbt.equals(storageWrapper.getSettingsHandler().getNbt())) {
			lastSettingsNbt = storageWrapper.getSettingsHandler().getNbt().copy();
			Optional<UUID> linkedStorageGroupId = getLinkedStorageGroupId();
			if (player instanceof ServerPlayer serverPlayer && linkedStorageGroupId.isPresent()) {
				UUID groupId = linkedStorageGroupId.get();
				PacketDistributor.sendToPlayer(serverPlayer, LinkedStorageBackpackContentsPayload.createSnapshot(serverPlayer.serverLevel(), groupId));
				return;
			}

			storageWrapper.getContentsUuid().ifPresent(uuid -> {
				CompoundTag settingsContents = new CompoundTag();
				CompoundTag settingsNbt = storageWrapper.getSettingsHandler().getNbt();
				if (!settingsNbt.isEmpty()) {
					settingsContents.put(BackpackSettingsHandler.SETTINGS_TAG, settingsNbt);
					if (player instanceof ServerPlayer serverPlayer) {
						PacketDistributor.sendToPlayer(serverPlayer, new BackpackContentsPayload(uuid, settingsContents));
					}
				}
			});
		}
	}

	@Override
	public BackpackContext getBackpackContext() {
		return backpackContext;
	}

	@Override
	public void removed(Player player) {
		super.removed(player);
		if (backpackContext.getType() != BackpackContext.ContextType.BLOCK_BACKPACK
				&& storageWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
			linkedStorageBackpackWrapper.close();
		}
	}

	private Optional<UUID> getLinkedStorageGroupId() {
		LinkedStorageEndpointData endpoint = storageWrapper.getBackpack().get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		return Optional.ofNullable(endpoint).map(LinkedStorageEndpointData::groupId);
	}
}
