package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.ImmutableMap;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncClientInfoPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public abstract class BackpackContext {

	public static final String SUBBACKPACK_DISPLAY_NAME_PREFIX = "... > ";

	public abstract Optional<IStorageWrapper> getParentBackpackWrapper(Player player);

	public abstract boolean shouldLockBackpackSlot(Player player);

	public abstract IBackpackWrapper getBackpackWrapper(Player player);

	public abstract int getBackpackSlotIndex();

	public abstract BackpackContext getSubBackpackContext(int subBackpackSlotIndex, boolean saveAfterOpen);

	public abstract BackpackContext getParentBackpackContext();

	public abstract ContextType getType();

	public void toBuffer(FriendlyByteBuf buffer) {
		getType().toBuffer(buffer);
		addToBuffer(buffer);
	}

	public abstract void addToBuffer(FriendlyByteBuf buffer);

	public abstract boolean canInteractWith(Player player);

	public BlockPos getBackpackPosition(Player playerEntity) {
		return playerEntity.blockPosition();
	}

	public Component getDisplayName(Player player) {
		return getBackpackWrapper(player).getBackpack().getHoverName();
	}

	public abstract void onUpgradeChanged(Player player);

	protected void syncOpenBackpackClientInfo(Player player) {
		if (player.level().isClientSide || !(player instanceof ServerPlayer serverPlayer)) {
			return;
		}

		IStorageWrapper backpackWrapper = getBackpackWrapper(player);
		PacketDistributor.sendToPlayer(serverPlayer, new SyncClientInfoPayload(-1, backpackWrapper.getRenderInfo().getNbt().copy(), backpackWrapper.getColumnsTaken()));
	}

	public Optional<Entity> getOwnerPlayer(Player player) {
		return Optional.of(player);
	}

	public static BackpackContext fromBuffer(FriendlyByteBuf buffer, Level level) {
		ContextType type = ContextType.fromBuffer(buffer);
		return switch (type) {
			case BLOCK_BACKPACK -> Block.fromBuffer(buffer);
			case BLOCK_SUB_BACKPACK -> BlockSubBackpack.fromBuffer(buffer);
			case ITEM_SUB_BACKPACK -> ItemSubBackpack.fromBuffer(buffer);
			case ITEM_BACKPACK -> Item.fromBuffer(buffer);
			case ANOTHER_PLAYER_BACKPACK -> AnotherPlayer.fromBuffer(buffer, level);
			case ANOTHER_PLAYER_SUB_BACKPACK -> AnotherPlayerSubBackpack.fromBuffer(buffer, level);
		};
	}

	public boolean wasOpenFromInventory() {
		return false;
	}

	public boolean shouldSaveAfterOpen() {
		return false;
	}

	public void saveBackpackStack() {
		//noop by default
	}

	public enum ContextType {
		BLOCK_BACKPACK(0),
		BLOCK_SUB_BACKPACK(1),
		ITEM_BACKPACK(2),
		ITEM_SUB_BACKPACK(3),
		ANOTHER_PLAYER_BACKPACK(4),
		ANOTHER_PLAYER_SUB_BACKPACK(5);

		private final int id;

		ContextType(int id) {
			this.id = id;
		}

		public void toBuffer(FriendlyByteBuf buffer) {
			buffer.writeShort(id);
		}

		private static final Map<Integer, ContextType> ID_CONTEXTS;

		static {
			ImmutableMap.Builder<Integer, ContextType> builder = new ImmutableMap.Builder<>();
			for (ContextType value : ContextType.values()) {
				builder.put(value.id, value);
			}
			ID_CONTEXTS = builder.build();
		}

		public static ContextType fromBuffer(FriendlyByteBuf buffer) {
			return ID_CONTEXTS.getOrDefault((int) buffer.readShort(), ContextType.ITEM_BACKPACK);
		}
	}

	public static class Item extends BackpackContext {
		protected final String handlerName;
		protected final String identifier;
		protected final int backpackSlotIndex;
		private final boolean openFromInventory;

		public Item(String handlerName, int backpackSlotIndex) {
			this(handlerName, "", backpackSlotIndex);
		}

		public Item(String handlerName, String identifier, int backpackSlotIndex) {
			this(handlerName, identifier, backpackSlotIndex, false);
		}

		public Item(String handlerName, String identifier, int backpackSlotIndex, boolean openFromInventory) {
			this.handlerName = handlerName;
			this.identifier = identifier;
			this.backpackSlotIndex = backpackSlotIndex;
			this.openFromInventory = openFromInventory;
		}

		@Override
		public boolean wasOpenFromInventory() {
			return openFromInventory;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			return Optional.empty();
		}

		@Override
		public boolean shouldLockBackpackSlot(Player player) {
			return PlayerInventoryProvider.get().getPlayerInventoryHandler(handlerName).map(PlayerInventoryHandler::isVisibleInGui).orElse(false);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			Optional<PlayerInventoryHandler> inventoryHandler = PlayerInventoryProvider.get().getPlayerInventoryHandler(handlerName);
			if (inventoryHandler.isEmpty()) {
				SophisticatedBackpacks.LOGGER.error("Error getting backpack wrapper - Unable to find inventory handler for \"{}\"", handlerName);
				return IBackpackWrapper.Noop.INSTANCE;
			}
			ItemStack backpackStack = inventoryHandler.get().getStackInSlot(player, identifier, backpackSlotIndex);
			if (backpackStack.getItem() instanceof BackpackItem) {
				return BackpackWrapper.fromStack(backpackStack);
			}
			SophisticatedBackpacks.LOGGER.error("Error getting backpack wrapper - Stack isn't a backpack");
			return IBackpackWrapper.Noop.INSTANCE;
		}

		@Override
		public void onUpgradeChanged(Player player) {
			if (!player.level().isClientSide) {
				IStorageWrapper backpackWrapper = getBackpackWrapper(player);
				//copying render info nbt because in single player the packet just gets handed over to client instead of actually buffer being used and making a copy of the nbt
				// which resulted in issues where client would happily modify nbt instance which is used on server as well and follow up updates on client would also mess up
				// as they are free to clear state which caused the new cleared state write into the already existing nbt and server nbt as all were the same instanced
				CompoundTag modificationSafeRenderInfoNbt = backpackWrapper.getRenderInfo().getNbt().copy();
				if (player instanceof ServerPlayer serverPlayer) {
					int payloadSlotIndex = handlerName.equals(PlayerInventoryProvider.MAIN_INVENTORY) ? backpackSlotIndex : -1;
					PacketDistributor.sendToPlayer(serverPlayer, new SyncClientInfoPayload(payloadSlotIndex, modificationSafeRenderInfoNbt, backpackWrapper.getColumnsTaken()));
				}
			}
		}

		@Override
		public int getBackpackSlotIndex() {
			return backpackSlotIndex;
		}

		@Override
		public BackpackContext getSubBackpackContext(int subBackpackSlotIndex, boolean saveAfterOpen) {
			return new ItemSubBackpack(handlerName, identifier, backpackSlotIndex, openFromInventory, subBackpackSlotIndex, saveAfterOpen);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return this;
		}

		@Override
		public ContextType getType() {
			return ContextType.ITEM_BACKPACK;
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf buffer) {
			return new Item(buffer.readUtf(), buffer.readUtf(), buffer.readInt(), buffer.readBoolean());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf buffer) {
			buffer.writeUtf(handlerName);
			buffer.writeUtf(identifier);
			buffer.writeInt(backpackSlotIndex);
			buffer.writeBoolean(openFromInventory);
		}

		@Override
		public boolean canInteractWith(Player player) {
			return true;
		}
	}

	public static class ItemSubBackpack extends Item {
		private final int subBackpackSlotIndex;
		private final boolean saveAfterOpen;
		@Nullable
		private IStorageWrapper parentWrapper;

		public ItemSubBackpack(String handlerName, String identifier, int backpackSlotIndex, boolean parentOpenFromInventory, int subBackpackSlotIndex, boolean saveAfterOpen) {
			super(handlerName, identifier, backpackSlotIndex, parentOpenFromInventory);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
			this.saveAfterOpen = saveAfterOpen;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				if (player.containerMenu instanceof BackpackContainer backpackContainer && matchesParentContext(backpackContainer.getBackpackContext())) {
					parentWrapper = backpackContainer.getStorageWrapper();
				} else {
					parentWrapper = super.getBackpackWrapper(player);
				}
			}
			return Optional.of(parentWrapper);
		}

		private boolean matchesParentContext(BackpackContext backpackContext) {
			return backpackContext instanceof Item itemContext && itemContext.getType() == ContextType.ITEM_BACKPACK && itemContext.handlerName.equals(handlerName) && itemContext.identifier.equals(identifier) && itemContext.backpackSlotIndex == backpackSlotIndex;
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(player)
					.map(parent -> {
						ItemStack stackInSlot = parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex);
						if (!(stackInSlot.getItem() instanceof BackpackItem)) {
							return IBackpackWrapper.Noop.INSTANCE;
						}
						return BackpackWrapper.fromStack(stackInSlot);
					}).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf buffer) {
			return new ItemSubBackpack(buffer.readUtf(), buffer.readUtf(), buffer.readInt(), buffer.readBoolean(), buffer.readInt(), buffer.readBoolean());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf buffer) {
			super.addToBuffer(buffer);
			buffer.writeInt(subBackpackSlotIndex);
			buffer.writeBoolean(saveAfterOpen);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new Item(handlerName, identifier, backpackSlotIndex, super.wasOpenFromInventory());
		}

		@Override
		public ContextType getType() {
			return ContextType.ITEM_SUB_BACKPACK;
		}

		@Override
		public Component getDisplayName(Player player) {
			return Component.literal(SUBBACKPACK_DISPLAY_NAME_PREFIX + super.getDisplayName(player).getString());
		}

		@Override
		public void onUpgradeChanged(Player player) {
			syncOpenBackpackClientInfo(player);
		}

		@Override
		public boolean shouldSaveAfterOpen() {
			return saveAfterOpen;
		}

		@Override
		public void saveBackpackStack() {
			if (parentWrapper != null) {
				parentWrapper.getInventoryHandler().setStackInSlot(subBackpackSlotIndex, parentWrapper.getInventoryHandler().getStackInSlot(subBackpackSlotIndex));
				parentWrapper.getInventoryHandler().saveInventory();
			}
		}
	}

	public static class Block extends BackpackContext {
		protected final BlockPos pos;

		public Block(BlockPos pos) {
			this.pos = pos;
		}

		@Override
		public BlockPos getBackpackPosition(Player playerEntity) {
			return pos;
		}

		@Override
		public void onUpgradeChanged(Player player) {
			if (!player.level().isClientSide) {
				WorldHelper.getBlockEntity(player.level(), pos, BackpackBlockEntity.class).ifPresent(backpackBlockEntity -> {
					backpackBlockEntity.refreshRenderState();
					if (getType() == ContextType.BLOCK_BACKPACK && player instanceof ServerPlayer serverPlayer) {
						IBackpackWrapper backpackWrapper = backpackBlockEntity.getBackpackWrapper();
						PacketDistributor.sendToPlayer(serverPlayer, new SyncClientInfoPayload(-1, backpackWrapper.getRenderInfo().getNbt().copy(), backpackWrapper.getColumnsTaken()));
					}
				});
			}
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			return Optional.empty();
		}

		@Override
		public boolean shouldLockBackpackSlot(Player player) {
			return false;
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return WorldHelper.getBlockEntity(player.level(), pos, BackpackBlockEntity.class).map(BackpackBlockEntity::getBackpackWrapper).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public int getBackpackSlotIndex() {
			return -1;
		}

		@Override
		public BackpackContext getSubBackpackContext(int subBackpackSlotIndex, boolean saveAfterOpen) {
			return new BlockSubBackpack(pos, subBackpackSlotIndex, saveAfterOpen);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return this;
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf buffer) {
			return new Block(BlockPos.of(buffer.readLong()));
		}

		@Override
		public void addToBuffer(FriendlyByteBuf buffer) {
			buffer.writeLong(pos.asLong());
		}

		@Override
		public boolean canInteractWith(Player player) {
			return player.level().getBlockEntity(pos) instanceof BackpackBlockEntity
					&& (player.distanceToSqr(pos.getX() + 0.5D, pos.getY() + 0.5D, pos.getZ() + 0.5D) <= 64.0D);
		}

		@Override
		public ContextType getType() {
			return ContextType.BLOCK_BACKPACK;
		}

		@Override
		public Optional<Entity> getOwnerPlayer(Player player) {
			return Optional.empty();
		}
	}

	public static class BlockSubBackpack extends Block {
		private final int subBackpackSlotIndex;
		private final boolean saveAfterOpen;
		@Nullable
		private IStorageWrapper parentWrapper;

		public BlockSubBackpack(BlockPos pos, int subBackpackSlotIndex, boolean saveAfterOpen) {
			super(pos);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
			this.saveAfterOpen = saveAfterOpen;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				if (player.containerMenu instanceof BackpackContainer backpackContainer && matchesParentContext(backpackContainer.getBackpackContext())) {
					parentWrapper = backpackContainer.getStorageWrapper();
				} else {
					parentWrapper = super.getBackpackWrapper(player);
				}
			}
			return Optional.of(parentWrapper);
		}

		private boolean matchesParentContext(BackpackContext backpackContext) {
			return backpackContext instanceof Block blockContext && blockContext.getType() == ContextType.BLOCK_BACKPACK && blockContext.pos.equals(pos);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(player)
					.map(parent -> {
						ItemStack stackInSlot = parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex);
						if (!(stackInSlot.getItem() instanceof BackpackItem)) {
							return IBackpackWrapper.Noop.INSTANCE;
						}
						return BackpackWrapper.fromStack(stackInSlot);
					}).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf buffer) {
			return new BlockSubBackpack(BlockPos.of(buffer.readLong()), buffer.readInt(), buffer.readBoolean());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf buffer) {
			super.addToBuffer(buffer);
			buffer.writeInt(subBackpackSlotIndex);
			buffer.writeBoolean(saveAfterOpen);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new Block(pos);
		}

		@Override
		public ContextType getType() {
			return ContextType.BLOCK_SUB_BACKPACK;
		}

		@Override
		public Component getDisplayName(Player player) {
			return Component.literal(SUBBACKPACK_DISPLAY_NAME_PREFIX + super.getDisplayName(player).getString());
		}

		@Override
		public void onUpgradeChanged(Player player) {
			syncOpenBackpackClientInfo(player);
		}

		@Override
		public boolean shouldSaveAfterOpen() {
			return saveAfterOpen;
		}

		@Override
		public void saveBackpackStack() {
			if (parentWrapper != null) {
				parentWrapper.getInventoryHandler().setStackInSlot(subBackpackSlotIndex, parentWrapper.getInventoryHandler().getStackInSlot(subBackpackSlotIndex));
				parentWrapper.getInventoryHandler().saveInventory();
			}
		}
	}

	public static class AnotherPlayer extends Item {
		protected final Player otherPlayer;

		public AnotherPlayer(String handlerName, String identifier, int backpackSlotIndex, Player otherPlayer) {
			super(handlerName, identifier, backpackSlotIndex);
			this.otherPlayer = otherPlayer;
		}

		@Override
		public boolean shouldLockBackpackSlot(Player player) {
			return false;
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return super.getBackpackWrapper(otherPlayer);
		}

		@Override
		public BackpackContext getSubBackpackContext(int subBackpackSlotIndex, boolean saveAfterOpen) {
			return new AnotherPlayerSubBackpack(otherPlayer, handlerName, identifier, backpackSlotIndex, subBackpackSlotIndex, saveAfterOpen);
		}

		@Override
		public void addToBuffer(FriendlyByteBuf buffer) {
			buffer.writeInt(otherPlayer.getId());
			buffer.writeUtf(handlerName);
			buffer.writeUtf(identifier);
			buffer.writeInt(backpackSlotIndex);
		}

		@Override
		public boolean canInteractWith(Player player) {
			return player.distanceTo(otherPlayer) < 8;
		}

		@Override
		public ContextType getType() {
			return ContextType.ANOTHER_PLAYER_BACKPACK;
		}

		@Override
		public Component getDisplayName(Player player) {
			return super.getDisplayName(otherPlayer);
		}

		@Override
		public void onUpgradeChanged(Player player) {
			syncOpenBackpackClientInfo(player);
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf buffer, Level level) {
			int playerId = buffer.readInt();
			Player otherPlayer = (Player) level.getEntity(playerId);

			return new AnotherPlayer(buffer.readUtf(), buffer.readUtf(), buffer.readInt(), Objects.requireNonNull(otherPlayer));
		}

		@Override
		public Optional<Entity> getOwnerPlayer(Player player) {
			return Optional.of(otherPlayer);
		}
	}

	public static class AnotherPlayerSubBackpack extends AnotherPlayer {
		private final int subBackpackSlotIndex;
		private final boolean saveAfterOpen;
		@Nullable
		private IStorageWrapper parentWrapper;

		public AnotherPlayerSubBackpack(Player otherPlayer, String handlerName, String identifier, int backpackSlotIndex, int subBackpackSlotIndex, boolean saveAfterOpen) {
			super(handlerName, identifier, backpackSlotIndex, otherPlayer);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
			this.saveAfterOpen = saveAfterOpen;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				if (player.containerMenu instanceof BackpackContainer backpackContainer && matchesParentContext(backpackContainer.getBackpackContext())) {
					parentWrapper = backpackContainer.getStorageWrapper();
				} else {
					parentWrapper = super.getBackpackWrapper(player);
				}
			}
			return Optional.of(parentWrapper);
		}

		private boolean matchesParentContext(BackpackContext backpackContext) {
			return backpackContext instanceof AnotherPlayer anotherPlayerContext && anotherPlayerContext.getType() == ContextType.ANOTHER_PLAYER_BACKPACK && anotherPlayerContext.otherPlayer == otherPlayer && anotherPlayerContext.handlerName.equals(handlerName) && anotherPlayerContext.identifier.equals(identifier) && anotherPlayerContext.backpackSlotIndex == backpackSlotIndex;
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(player)
					.map(parent -> BackpackWrapper.fromExistingData(parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex))
							.orElse(IBackpackWrapper.Noop.INSTANCE)).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public void addToBuffer(FriendlyByteBuf buffer) {
			super.addToBuffer(buffer);
			buffer.writeInt(subBackpackSlotIndex);
			buffer.writeBoolean(saveAfterOpen);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new AnotherPlayer(handlerName, identifier, backpackSlotIndex, otherPlayer);
		}

		@Override
		public ContextType getType() {
			return ContextType.ANOTHER_PLAYER_SUB_BACKPACK;
		}

		@Override
		public Component getDisplayName(Player player) {
			return Component.literal(SUBBACKPACK_DISPLAY_NAME_PREFIX + super.getDisplayName(player).getString());
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf buffer, Level level) {
			int playerId = buffer.readInt();
			Player otherPlayer = (Player) level.getEntity(playerId);

			return new AnotherPlayerSubBackpack(Objects.requireNonNull(otherPlayer), buffer.readUtf(), buffer.readUtf(), buffer.readInt(), buffer.readInt(), buffer.readBoolean());
		}

		@Override
		public void onUpgradeChanged(Player player) {
			syncOpenBackpackClientInfo(player);
		}

		@Override
		public boolean shouldSaveAfterOpen() {
			return saveAfterOpen;
		}

		@Override
		public void saveBackpackStack() {
			if (parentWrapper != null) {
				parentWrapper.getInventoryHandler().setStackInSlot(subBackpackSlotIndex, parentWrapper.getInventoryHandler().getStackInSlot(subBackpackSlotIndex));
				parentWrapper.getInventoryHandler().saveInventory();
			}
		}
	}
}
