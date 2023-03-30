package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.ImmutableMap;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.util.LazyOptional;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncClientInfoMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public abstract class BackpackContext {
	public abstract Optional<IStorageWrapper> getParentBackpackWrapper(Player player);

	public abstract boolean shouldLockBackpackSlot(Player player);

	public abstract IBackpackWrapper getBackpackWrapper(Player player);

	public abstract int getBackpackSlotIndex();

	public abstract BackpackContext getSubBackpackContext(int subBackpackSlotIndex);

	public abstract BackpackContext getParentBackpackContext();

	public abstract ContextType getType();

	public void toBuffer(FriendlyByteBuf packetBuffer) {
		getType().toBuffer(packetBuffer);
		addToBuffer(packetBuffer);
	}

	public abstract void addToBuffer(FriendlyByteBuf packetBuffer);

	public abstract boolean canInteractWith(Player player);

	public BlockPos getBackpackPosition(Player playerEntity) {
		return playerEntity.blockPosition();
	}

	public Component getDisplayName(Player player) {
		return getBackpackWrapper(player).getBackpack().getHoverName();
	}

	public abstract void onUpgradeChanged(Player player);

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
		protected String identifier;
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
			LazyOptional<IBackpackWrapper> backpackWrapper = inventoryHandler.get().getStackInSlot(player, identifier, backpackSlotIndex).getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
			if (!backpackWrapper.isPresent()) {
				SophisticatedBackpacks.LOGGER.error("Error getting backpack wrapper - Unable to find backpack at slot index {} in \"{}\" inventory handler", backpackSlotIndex, handlerName);
				return IBackpackWrapper.Noop.INSTANCE;
			}
			return backpackWrapper.orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public void onUpgradeChanged(Player player) {
			if (!player.level.isClientSide && handlerName.equals(PlayerInventoryProvider.MAIN_INVENTORY)) {
				IStorageWrapper backpackWrapper = getBackpackWrapper(player);
				SBPPacketHandler.INSTANCE.sendToClient((ServerPlayer) player, new SyncClientInfoMessage(backpackSlotIndex, backpackWrapper.getRenderInfo().getNbt(), backpackWrapper.getColumnsTaken()));
			}
		}

		@Override
		public int getBackpackSlotIndex() {
			return backpackSlotIndex;
		}

		@Override
		public BackpackContext getSubBackpackContext(int subBackpackSlotIndex) {
			return new ItemSubBackpack(handlerName, identifier, backpackSlotIndex, openFromInventory, subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return this;
		}

		@Override
		public ContextType getType() {
			return ContextType.ITEM_BACKPACK;
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer) {
			return new BackpackContext.Item(packetBuffer.readUtf(), packetBuffer.readUtf(), packetBuffer.readInt(), packetBuffer.readBoolean());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			packetBuffer.writeUtf(handlerName);
			packetBuffer.writeUtf(identifier);
			packetBuffer.writeInt(backpackSlotIndex);
			packetBuffer.writeBoolean(openFromInventory);
		}

		@Override
		public boolean canInteractWith(Player player) {
			return true;
		}
	}

	public static class ItemSubBackpack extends Item {
		private final int subBackpackSlotIndex;
		@Nullable
		private IStorageWrapper parentWrapper;

		public ItemSubBackpack(String handlerName, String identifier, int backpackSlotIndex, boolean parentOpenFromInventory, int subBackpackSlotIndex) {
			super(handlerName, identifier, backpackSlotIndex, parentOpenFromInventory);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				parentWrapper = super.getBackpackWrapper(player);
			}
			return Optional.of(parentWrapper);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(player).map(parent -> parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.orElse(IBackpackWrapper.Noop.INSTANCE)).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer) {
			return new BackpackContext.ItemSubBackpack(packetBuffer.readUtf(), packetBuffer.readUtf(), packetBuffer.readInt(), packetBuffer.readBoolean(), packetBuffer.readInt());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			super.addToBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new BackpackContext.Item(handlerName, identifier, backpackSlotIndex, super.wasOpenFromInventory());
		}

		@Override
		public ContextType getType() {
			return ContextType.ITEM_SUB_BACKPACK;
		}

		@Override
		public Component getDisplayName(Player player) {
			return new TextComponent("... > " + super.getDisplayName(player).getString());
		}

		@Override
		public void onUpgradeChanged(Player player) {
			//noop
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
			if (!player.level.isClientSide) {
				WorldHelper.getBlockEntity(player.level, pos, BackpackBlockEntity.class).ifPresent(BackpackBlockEntity::refreshRenderState);
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
			return WorldHelper.getBlockEntity(player.level, pos, BackpackBlockEntity.class).map(BackpackBlockEntity::getBackpackWrapper).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public int getBackpackSlotIndex() {
			return -1;
		}

		@Override
		public BackpackContext getSubBackpackContext(int subBackpackSlotIndex) {
			return new BlockSubBackpack(pos, subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return this;
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer) {
			return new BackpackContext.Block(BlockPos.of(packetBuffer.readLong()));
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			packetBuffer.writeLong(pos.asLong());
		}

		@Override
		public boolean canInteractWith(Player player) {
			return player.level.getBlockEntity(pos) instanceof BackpackBlockEntity
					&& (player.distanceToSqr(pos.getX() + 0.5D, pos.getY() + 0.5D, pos.getZ() + 0.5D) <= 64.0D);
		}

		@Override
		public ContextType getType() {
			return ContextType.BLOCK_BACKPACK;
		}
	}

	public static class BlockSubBackpack extends Block {
		private final int subBackpackSlotIndex;
		@Nullable
		private IStorageWrapper parentWrapper;

		public BlockSubBackpack(BlockPos pos, int subBackpackSlotIndex) {
			super(pos);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				parentWrapper = super.getBackpackWrapper(player);
			}
			return Optional.of(parentWrapper);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(player).map(parent -> parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.orElse(IBackpackWrapper.Noop.INSTANCE)).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer) {
			return new BackpackContext.BlockSubBackpack(BlockPos.of(packetBuffer.readLong()), packetBuffer.readInt());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			super.addToBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new BackpackContext.Block(pos);
		}

		@Override
		public ContextType getType() {
			return ContextType.BLOCK_SUB_BACKPACK;
		}

		@Override
		public Component getDisplayName(Player player) {
			return new TextComponent("... > " + super.getDisplayName(player).getString());
		}

		@Override
		public void onUpgradeChanged(Player player) {
			//noop
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
		public BackpackContext getSubBackpackContext(int subBackpackSlotIndex) {
			return new AnotherPlayerSubBackpack(otherPlayer, handlerName, identifier, backpackSlotIndex, subBackpackSlotIndex);
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			packetBuffer.writeInt(otherPlayer.getId());
			packetBuffer.writeUtf(handlerName);
			packetBuffer.writeUtf(identifier);
			packetBuffer.writeInt(backpackSlotIndex);
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

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer, Level level) {
			int playerId = packetBuffer.readInt();
			Player otherPlayer = (Player) level.getEntity(playerId);

			return new BackpackContext.AnotherPlayer(packetBuffer.readUtf(), packetBuffer.readUtf(), packetBuffer.readInt(), Objects.requireNonNull(otherPlayer));
		}
	}

	public static class AnotherPlayerSubBackpack extends AnotherPlayer {
		private final int subBackpackSlotIndex;
		@Nullable
		private IStorageWrapper parentWrapper;

		public AnotherPlayerSubBackpack(Player otherPlayer, String handlerName, String identifier, int backpackSlotIndex, int subBackpackSlotIndex) {
			super(handlerName, identifier, backpackSlotIndex, otherPlayer);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				parentWrapper = super.getBackpackWrapper(player);
			}
			return Optional.of(parentWrapper);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(otherPlayer).map(parent -> parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.orElse(IBackpackWrapper.Noop.INSTANCE)).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			super.addToBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new BackpackContext.AnotherPlayer(handlerName, identifier, backpackSlotIndex, otherPlayer);
		}

		@Override
		public ContextType getType() {
			return ContextType.ANOTHER_PLAYER_SUB_BACKPACK;
		}

		@Override
		public Component getDisplayName(Player player) {
			return new TextComponent("... > " + super.getDisplayName(player).getString());
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer, Level level) {
			int playerId = packetBuffer.readInt();
			Player otherPlayer = (Player) level.getEntity(playerId);

			return new BackpackContext.AnotherPlayerSubBackpack(Objects.requireNonNull(otherPlayer), packetBuffer.readUtf(), packetBuffer.readUtf(), packetBuffer.readInt(), packetBuffer.readInt());
		}

		@Override
		public void onUpgradeChanged(Player player) {
			//noop
		}
	}
}
