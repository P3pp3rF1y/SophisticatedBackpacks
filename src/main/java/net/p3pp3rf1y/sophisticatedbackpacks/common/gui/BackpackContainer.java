package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.UUIDDeduplicator;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedcore.common.gui.ISyncedContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SophisticatedMenuProvider;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageContainerMenuBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IClientStorageContentsProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.util.NoopStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.BACKPACK_CONTAINER_TYPE;

public class BackpackContainer extends StorageContainerMenuBase<IBackpackWrapper> implements ISyncedContainer {
	private final BackpackContext backpackContext;

	public BackpackContainer(int windowId, Player player, BackpackContext backpackContext) {
		super(BACKPACK_CONTAINER_TYPE.get(), windowId, player, backpackContext.getBackpackWrapper(player), backpackContext.getParentBackpackWrapper(player).orElse(NoopStorageWrapper.INSTANCE), backpackContext.getBackpackSlotIndex(), backpackContext.shouldLockBackpackSlot(player));
		this.backpackContext = backpackContext;
		if (!player.level().isClientSide() && (backpackContext.getType() == BackpackContext.ContextType.ITEM_BACKPACK || backpackContext.getType() == BackpackContext.ContextType.ITEM_SUB_BACKPACK)) {
			storageWrapper.onInit(player.level());
		}

		storageWrapper.getContentsUuid().ifPresent(backpackUuid ->
		{
			ItemStack backpack = storageWrapper.getBackpack();
			BackpackAccessLogger.logPlayerAccess(player, backpack.getItem(), backpackUuid, backpack.getHoverName().getString(),
					storageWrapper.getMainColor(), storageWrapper.getAccentColor(), storageWrapper.getColumnsTaken());

			if (!player.level().isClientSide()) {
				UUIDDeduplicator.checkForDuplicateBackpacksAndRemoveTheirUUID(player, backpackUuid, storageWrapper.getBackpack());
			}
		});

		if (backpackContext.shouldSaveAfterOpen()) {
			backpackContext.saveBackpackStack();
		}

		if (!player.level().isClientSide) {
			getBlockPosition().flatMap(pos -> WorldHelper.getBlockEntity(player.level(), pos, BackpackBlockEntity.class))
					.ifPresent(backpackBlockEntity -> backpackBlockEntity.startOpen(player));
		}
	}

	@Override
	public Optional<BlockPos> getBlockPosition() {
		BackpackContext.ContextType type = backpackContext.getType();
		if (type == BackpackContext.ContextType.BLOCK_BACKPACK || type == BackpackContext.ContextType.BLOCK_SUB_BACKPACK) {
			return Optional.of(backpackContext.getBackpackPosition(player));
		}
		return Optional.empty();
	}

	@Override
	public Optional<Entity> getEntity() {
		return backpackContext.getOwnerPlayer(player);
	}

	@Override
	protected void sendStorageSettingsToClient() {
		if (player.level().isClientSide) {
			return;
		}

		storageWrapper.getContentsUuid().ifPresent(uuid -> {
			CompoundTag settingsContents = new CompoundTag();
			CompoundTag settingsNbt = storageWrapper.getSettingsHandler().getNbt();
			if (!settingsNbt.isEmpty()) {
				settingsContents.put(BackpackSettingsHandler.SETTINGS_TAG, settingsNbt);
			}
			storageWrapper.getUpgradeHandler().getWrappersThatImplementFromMainStorage(IClientStorageContentsProvider.class).forEach(provider -> provider.addClientStorageContents(settingsContents));
			if (!settingsContents.isEmpty()) {
				if (player instanceof ServerPlayer serverPlayer) {
					PacketDistributor.sendToPlayer(serverPlayer, new BackpackContentsPayload(uuid, settingsContents));
				}
			}
		});
	}

	@Override
	protected void onStorageItemStackChanged(ItemStack stack) {
		storageWrapper.setBackpackStack(stack);
	}

	public void syncClientInfo(CompoundTag renderInfoNbt, int columnsTaken) {
		storageWrapper.getRenderInfo().deserializeFrom(renderInfoNbt);
		storageWrapper.setColumnsTaken(columnsTaken, false);
		storageWrapper.onContentsNbtUpdated();
		refreshAllSlots();
		onUpgradesChanged();
	}

	public boolean canApplyClientInfo(int slotIndex) {
		BackpackContext.ContextType type = backpackContext.getType();
		if (slotIndex == -1) {
			return switch (type) {
				case ITEM_BACKPACK, ITEM_SUB_BACKPACK, BLOCK_BACKPACK, BLOCK_SUB_BACKPACK, ANOTHER_PLAYER_BACKPACK, ANOTHER_PLAYER_SUB_BACKPACK -> true;
			};
		}

		return type == BackpackContext.ContextType.ITEM_BACKPACK && backpackContext.getBackpackSlotIndex() == slotIndex;
	}

	public void syncClientStorageContentsToClient() {
		sendStorageSettingsToClient();
		refreshAdditionalSlotInfo();
	}

	@Override
	protected StorageUpgradeSlot instantiateUpgradeSlot(UpgradeHandler upgradeHandler, int slotIndex) {
		return new BackpackUpgradeSlot(upgradeHandler, slotIndex);
	}

	@Override
	public boolean stillValid(Player player) {
		return backpackContext.canInteractWith(player);
	}

	@Override
	public void removed(Player player) {
		if (!player.level().isClientSide) {
			getBlockPosition().flatMap(pos -> WorldHelper.getBlockEntity(player.level(), pos, BackpackBlockEntity.class))
					.ifPresent(backpackBlockEntity -> backpackBlockEntity.stopOpen(player));
		}

		super.removed(player);
	}

	public static BackpackContainer fromBuffer(int windowId, Inventory playerInventory, FriendlyByteBuf buffer) {
		return new BackpackContainer(windowId, playerInventory.player, BackpackContext.fromBuffer(buffer, playerInventory.player.level()));
	}

	public BackpackContext getBackpackContext() {
		return backpackContext;
	}

	@Override
	public void openSettings() {
		if (isClientSide()) {
			sendToServer(data -> data.putString(ACTION_TAG, "openSettings"));
			return;
		}
		player.openMenu(new SophisticatedMenuProvider((w, p, pl) -> new BackpackSettingsContainerMenu(w, pl, backpackContext),
				Component.translatable(SBPTranslationHelper.INSTANCE.translGui("settings.title")), false), backpackContext::toBuffer);
	}

	@Override
	protected boolean storageItemHasChanged() {
		return backpackContext.getBackpackWrapper(player) != storageWrapper;
	}

	@Override
	protected void onUpgradeChanged() {
		super.onUpgradeChanged();
		backpackContext.onUpgradeChanged(player);
	}

	public class BackpackUpgradeSlot extends StorageUpgradeSlot {
		public BackpackUpgradeSlot(UpgradeHandler upgradeHandler, int slotIndex) {
			super(upgradeHandler, slotIndex);
		}

		@Override
		protected void onUpgradeChanged() {
			super.onUpgradeChanged();
			backpackContext.onUpgradeChanged(player);
		}
	}

	@Override
	public boolean detectSettingsChangeAndReload() {
		return storageWrapper.getContentsUuid().map(uuid -> {
			BackpackStorage storage = BackpackStorage.get();
			if (storage.removeUpdatedBackpackSettingsFlag(uuid)) {
				storageWrapper.getSettingsHandler().reloadFrom(storage.getOrCreateBackpackContents(uuid));
				return true;
			}
			return false;
		}).orElse(false);
	}

	@Override
	protected boolean shouldSlotItemBeDroppedFromStorage(Slot slot) {
		return slot.getItem().getItem() instanceof BackpackItem &&
				!storageWrapper.getInventoryHandler().isItemValid(0, slot.getItem());
	}
}
