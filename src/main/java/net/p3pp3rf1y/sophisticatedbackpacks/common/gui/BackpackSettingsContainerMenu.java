package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackSettingsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.network.LinkedStorageBackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainerMenu;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsContainerBase;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategory;

import java.util.Optional;
import java.util.UUID;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.SETTINGS_CONTAINER_TYPE;

public class BackpackSettingsContainerMenu extends SettingsContainerMenu<IBackpackWrapper> implements IContextAwareContainer {
	private static final ISettingsContainerFactory<MainSettingsCategory, BackpackMainSettingsContainer> MAIN_SETTINGS_CONTAINER_FACTORY_OVERRIDE = BackpackMainSettingsContainer::new;
	private final BackpackContext backpackContext;
	private ContainerContents.SettingsData lastSettingsData = null;

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
	protected <C extends ISettingsCategory<?, ?>, T extends SettingsContainerBase<C>> ISettingsContainerFactory<C, T> getSettingsContainerFactory(String name) {
		if (name.equals(MainSettingsCategory.NAME)) {
			// noinspection unchecked
			return (ISettingsContainerFactory<C, T>) MAIN_SETTINGS_CONTAINER_FACTORY_OVERRIDE;
		}

		return super.getSettingsContainerFactory(name);
	}

	@Override
	public void handlePacket(CompoundTag data) {
		super.handlePacket(data);
		if (!player.level().isClientSide() && storageWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
			linkedStorageBackpackWrapper.synchronizePhysicalProjection();
		}
	}

	@Override
	public void detectSettingsChangeAndReload() {
		if (player.level().isClientSide()) {
			Optional<UUID> linkedStorageGroupId = getLinkedStorageGroupId();
			if (linkedStorageGroupId.isPresent() && ClientLinkedStorageBackpackContents.removeUpdatedGroup(linkedStorageGroupId.get())) {
				ILinkedStorageContentsBinding contents = ClientLinkedStorageBackpackContents.getBinding(linkedStorageGroupId.get())
						.orElseThrow(() -> new IllegalStateException("Updated linked backpack group has no snapshot: " + linkedStorageGroupId.get()));
				storageWrapper.getSettingsHandler().reloadFrom(contents.contents().settings());
				if (storageWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
					linkedStorageBackpackWrapper.synchronizePhysicalProjection();
				}
				return;
			}
			if (linkedStorageGroupId.isPresent()) {
				return;
			}
			storageWrapper.getContentsUuid().ifPresent(uuid -> {
				BackpackStorage storage = BackpackStorage.get();
				if (storage.removeUpdatedBackpackSettingsFlag(uuid)) {
					storageWrapper.getSettingsHandler().reloadFrom(storage.getOrCreateBackpackContents(uuid).settings());
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
		if (player.level().isClientSide()) {
			return;
		}

		if (lastSettingsData == null || !lastSettingsData.equals(storageWrapper.getSettingsHandler().getSettingsData())) {
			lastSettingsData = storageWrapper.getSettingsHandler().getSettingsData().copy();
			Optional<UUID> linkedStorageGroupId = getLinkedStorageGroupId();
			if (player instanceof ServerPlayer serverPlayer && linkedStorageGroupId.isPresent()) {
				PacketDistributor.sendToPlayer(serverPlayer,
						LinkedStorageBackpackContentsPayload.createSnapshot(serverPlayer.level(), linkedStorageGroupId.get()));
				return;
			}

			storageWrapper.getContentsUuid().ifPresent(uuid -> {
				ContainerContents.SettingsData settingsData = storageWrapper.getSettingsHandler().getSettingsData();
				if (player instanceof ServerPlayer serverPlayer) {
					PacketDistributor.sendToPlayer(serverPlayer, new BackpackSettingsPayload(uuid, settingsData));
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
