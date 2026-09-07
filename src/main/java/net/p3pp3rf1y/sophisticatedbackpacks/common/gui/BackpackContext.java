package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.ImmutableMap;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.util.LazyOptional;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncClientInfoMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageVirtualHost;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nullable;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Supplier;

public abstract class BackpackContext {

	public static final String SUBBACKPACK_DISPLAY_NAME_PREFIX = "... > ";
	@Nullable
	protected IBackpackWrapper backpackWrapper;
	private boolean backpackWrapperHandedOff;

	public abstract Optional<IStorageWrapper> getParentBackpackWrapper(Player player);

	public abstract boolean shouldLockBackpackSlot(Player player);

	public abstract IBackpackWrapper getBackpackWrapper(Player player);

	public abstract int getBackpackSlotIndex();

	public abstract BackpackContext getSubBackpackContext(int subBackpackSlotIndex, boolean saveAfterOpen);

	public abstract BackpackContext getParentBackpackContext();

	public abstract ContextType getType();

	public void toBuffer(FriendlyByteBuf packetBuffer) {
		getType().toBuffer(packetBuffer);
		addToBuffer(packetBuffer);
		writeClientContextData(packetBuffer, null);
		packetBuffer.writeBoolean(false);
	}

	public void toBuffer(FriendlyByteBuf packetBuffer, Player player) {
		getType().toBuffer(packetBuffer);
		addToBuffer(packetBuffer);
		writeClientContextData(packetBuffer, player);
		writeLinkedStorageSnapshot(packetBuffer, player, getBackpackWrapper(player));
	}

	public static void writeLinkedStorageSnapshot(FriendlyByteBuf packetBuffer, Player player, IBackpackWrapper backpackWrapper) {
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpackWrapper.getBackpack());
		Optional<LinkedStorageSnapshot> snapshot = endpoint == null ? Optional.empty() : getLinkedStorageSnapshot(player, endpoint);
		packetBuffer.writeBoolean(snapshot.isPresent());
		snapshot.ifPresent(value -> writeLinkedStorageSnapshot(packetBuffer, value));
	}

	private static Optional<LinkedStorageSnapshot> getLinkedStorageSnapshot(Player player, LinkedStorageEndpointData endpoint) {
		if (!(player.level() instanceof ServerLevel serverLevel)) {
			return Optional.empty();
		}
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(serverLevel).manager();
		if (!manager.isEndpointMember(endpoint.groupId(), endpoint.endpointId())) {
			throw new IllegalStateException("Linked backpack endpoint is not registered in its group");
		}
		ILinkedStorageVirtualHost virtualHost = manager.resolveVirtualHost(endpoint.groupId())
				.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack host for group " + endpoint.groupId()));
		if (!(virtualHost instanceof IBackpackWrapper host)) {
			throw new IllegalStateException("Linked storage group " + endpoint.groupId() + " does not have a backpack host");
		}
		ILinkedStorageContentsBinding contents = manager.resolveContents(endpoint.groupId())
				.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack contents for group " + endpoint.groupId()));
		return Optional.of(new LinkedStorageSnapshot(endpoint.groupId(), manager.getRevision(endpoint.groupId()), contents.getContents().copy(),
				virtualHost.getLinkedStorageDisplayName().orElse(host.getDisplayName()),
				host.getInventoryHandler().getSlots() + host.getColumnsTaken() * host.getNumberOfSlotRows(), host.getUpgradeHandler().getSlots(),
				host.getColumnsTaken()));
	}

	private static void writeLinkedStorageSnapshot(FriendlyByteBuf buffer, LinkedStorageSnapshot snapshot) {
		buffer.writeUUID(snapshot.groupId());
		buffer.writeVarLong(snapshot.revision());
		buffer.writeNbt(snapshot.contents());
		buffer.writeComponent(snapshot.groupName());
		buffer.writeVarInt(snapshot.inventorySlots());
		buffer.writeVarInt(snapshot.upgradeSlots());
		buffer.writeVarInt(snapshot.columnsTaken());
	}

	public abstract void addToBuffer(FriendlyByteBuf packetBuffer);

	protected void writeClientContextData(FriendlyByteBuf packetBuffer, @Nullable Player player) {
		packetBuffer.writeBoolean(false);
	}

	protected void readClientContextData(FriendlyByteBuf packetBuffer) {
		if (packetBuffer.readBoolean()) {
			packetBuffer.readItem();
		}
	}

	public abstract boolean canInteractWith(Player player);

	public BlockPos getBackpackPosition(Player playerEntity) {
		return playerEntity.blockPosition();
	}

	public Component getDisplayName(Player player) {
		return getBackpackWrapper(player).getDisplayName();
	}

	public abstract void onUpgradeChanged(Player player);

	protected void syncOpenBackpackClientInfo(Player player) {
		if (player.level().isClientSide || !(player instanceof ServerPlayer serverPlayer)) {
			return;
		}

		IStorageWrapper backpackWrapper = getBackpackWrapper(player);
		SBPPacketHandler.INSTANCE.sendToClient(serverPlayer,
				new SyncClientInfoMessage(-1, backpackWrapper.getRenderInfo().getNbt().copy(), backpackWrapper.getColumnsTaken()));
	}

	public Optional<Entity> getOwnerPlayer(Player player) {
		return Optional.of(player);
	}

	public static BackpackContext fromBuffer(FriendlyByteBuf buffer, Level level) {
		ContextType type = ContextType.fromBuffer(buffer);
		BackpackContext context = switch (type) {
			case BLOCK_BACKPACK -> Block.fromBuffer(buffer);
			case BLOCK_SUB_BACKPACK -> BlockSubBackpack.fromBuffer(buffer);
			case ITEM_SUB_BACKPACK -> ItemSubBackpack.fromBuffer(buffer);
			case ITEM_BACKPACK -> Item.fromBuffer(buffer);
			case ANOTHER_PLAYER_BACKPACK -> AnotherPlayer.fromBuffer(buffer, level);
			case ANOTHER_PLAYER_SUB_BACKPACK -> AnotherPlayerSubBackpack.fromBuffer(buffer, level);
		};
		context.readClientContextData(buffer);
		readLinkedStorageSnapshot(buffer);
		return context;
	}

	public static void readLinkedStorageSnapshot(FriendlyByteBuf buffer) {
		if (buffer.readBoolean()) {
			UUID groupId = buffer.readUUID();
			long revision = buffer.readVarLong();
			CompoundTag contents = Objects.requireNonNull(buffer.readNbt());
			Component groupName = buffer.readComponent();
			ClientLinkedStorageBackpackContents.install(groupId, revision, contents, groupName, buffer.readVarInt(), buffer.readVarInt(), buffer.readVarInt());
		}
	}

	public boolean wasOpenFromInventory() {
		return false;
	}

	public boolean shouldSaveAfterOpen() {
		return false;
	}

	public void saveBackpackStack() {
		// noop by default
	}

	public void handoffBackpackWrapper() {
		backpackWrapperHandedOff = true;
	}

	public void setParentBackpackWrapper(IStorageWrapper parentWrapper) {
		// noop for first-level contexts
	}

	public void releaseBackpackWrapper() {
		if (backpackWrapperHandedOff) {
			backpackWrapperHandedOff = false;
			return;
		}
		closeBackpackWrapper();
	}

	protected void closeBackpackWrapper() {
		if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
			linkedStorageBackpackWrapper.close();
		}
		backpackWrapper = null;
	}

	protected IBackpackWrapper getOrCreateBackpackWrapper(Player player, ItemStack stack, Supplier<IBackpackWrapper> ordinaryWrapper) {
		if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper
				&& BackpackLinkedStorageResolver.hasSameEndpoint(backpackWrapper.getBackpack(), stack)) {
			linkedStorageBackpackWrapper.rebindPhysicalBackpack(stack);
			return linkedStorageBackpackWrapper;
		}
		if (backpackWrapper == null || backpackWrapper.getBackpack() != stack) {
			closeBackpackWrapper();
			backpackWrapper = BackpackLinkedStorageResolver.resolve(player.level(), stack).orElseGet(ordinaryWrapper);
			if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
				linkedStorageBackpackWrapper.setCanonicalContentsChangedHandler(() -> onLinkedBackpackProjectionChanged(player));
			}
		}
		return backpackWrapper;
	}

	protected void onLinkedBackpackProjectionChanged(Player player) {
		saveBackpackStack();
		onUpgradeChanged(player);
	}

	public enum ContextType {
		BLOCK_BACKPACK(0), BLOCK_SUB_BACKPACK(1), ITEM_BACKPACK(2), ITEM_SUB_BACKPACK(3), ANOTHER_PLAYER_BACKPACK(4), ANOTHER_PLAYER_SUB_BACKPACK(5);

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
			ItemStack stack = inventoryHandler.get().getStackInSlot(player, identifier, backpackSlotIndex);
			if (stack.isEmpty()) {
				return IBackpackWrapper.Noop.INSTANCE;
			}

			LazyOptional<IBackpackWrapper> capabilityBackpackWrapper = stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
			if (!capabilityBackpackWrapper.isPresent()) {
				SophisticatedBackpacks.LOGGER.error("Error getting backpack wrapper - Unable to find backpack at slot index {} in \"{}\" inventory handler",
						backpackSlotIndex, handlerName);
				return IBackpackWrapper.Noop.INSTANCE;
			}
			return getOrCreateBackpackWrapper(player, stack, () -> capabilityBackpackWrapper.orElse(IBackpackWrapper.Noop.INSTANCE));
		}

		private void syncLinkedBackpackRender(Player player) {
			if (player.level().isClientSide || !(player instanceof ServerPlayer)) {
				return;
			}
			syncOpenBackpackClientInfo(player);
			player.inventoryMenu.broadcastChanges();
		}

		@Override
		protected void onLinkedBackpackProjectionChanged(Player player) {
			syncLinkedBackpackRender(player);
		}

		@Override
		public void onUpgradeChanged(Player player) {
			if (!player.level().isClientSide) {
				IStorageWrapper backpackWrapper = getBackpackWrapper(player);
				int payloadSlotIndex = handlerName.equals(PlayerInventoryProvider.MAIN_INVENTORY) ? backpackSlotIndex : -1;
				SBPPacketHandler.INSTANCE.sendToClient((ServerPlayer) player,
						new SyncClientInfoMessage(payloadSlotIndex, backpackWrapper.getRenderInfo().getNbt().copy(), backpackWrapper.getColumnsTaken()));
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
		private final boolean saveAfterOpen;
		@Nullable
		private IStorageWrapper parentWrapper;
		private ItemStack clientChildBackpack = ItemStack.EMPTY;

		public ItemSubBackpack(String handlerName, String identifier, int backpackSlotIndex, boolean parentOpenFromInventory, int subBackpackSlotIndex,
				boolean saveAfterOpen) {
			super(handlerName, identifier, backpackSlotIndex, parentOpenFromInventory);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
			this.saveAfterOpen = saveAfterOpen;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				parentWrapper = new BackpackContext.Item(handlerName, identifier, backpackSlotIndex, super.wasOpenFromInventory()).getBackpackWrapper(player);
			}
			return Optional.of(parentWrapper);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(player).map(parent -> {
				ItemStack stack = parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex);
				if (stack.isEmpty() && player.level().isClientSide && !clientChildBackpack.isEmpty()) {
					stack = clientChildBackpack;
				}
				ItemStack childStack = stack;
				return getOrCreateBackpackWrapper(player, childStack,
						() -> childStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).orElse(IBackpackWrapper.Noop.INSTANCE));
			}).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public void setParentBackpackWrapper(IStorageWrapper parentWrapper) {
			this.parentWrapper = parentWrapper;
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer) {
			return new BackpackContext.ItemSubBackpack(packetBuffer.readUtf(), packetBuffer.readUtf(), packetBuffer.readInt(), packetBuffer.readBoolean(),
					packetBuffer.readInt(), packetBuffer.readBoolean());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			super.addToBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
			packetBuffer.writeBoolean(saveAfterOpen);
		}

		@Override
		protected void writeClientContextData(FriendlyByteBuf packetBuffer, @Nullable Player player) {
			if (player == null) {
				super.writeClientContextData(packetBuffer, null);
				return;
			}
			packetBuffer.writeBoolean(true);
			packetBuffer.writeItem(getBackpackWrapper(player).getBackpack());
		}

		@Override
		protected void readClientContextData(FriendlyByteBuf packetBuffer) {
			if (packetBuffer.readBoolean()) {
				clientChildBackpack = packetBuffer.readItem();
			}
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
			if (parentWrapper != null && backpackWrapper != null && isCurrentSubBackpackStack(parentWrapper, subBackpackSlotIndex, backpackWrapper)) {
				persistSubBackpackStack(parentWrapper, subBackpackSlotIndex, backpackWrapper.getBackpack());
			}
		}

		@Override
		public void releaseBackpackWrapper() {
			super.releaseBackpackWrapper();
			if (parentWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
				linkedStorageBackpackWrapper.close();
			}
			parentWrapper = null;
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
						SBPPacketHandler.INSTANCE.sendToClient(serverPlayer,
								new SyncClientInfoMessage(-1, backpackWrapper.getRenderInfo().getNbt().copy(), backpackWrapper.getColumnsTaken()));
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
			return WorldHelper.getBlockEntity(player.level(), pos, BackpackBlockEntity.class).map(blockEntity -> {
				IBackpackWrapper backpackWrapper = blockEntity.getBackpackWrapper();
				LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpackWrapper.getBackpack());
				if (player.level().isClientSide && endpoint != null && ClientLinkedStorageBackpackContents.getStorageSize(endpoint.groupId()).isPresent()) {
					return BackpackLinkedStorageResolver.resolveOrCreate(player.level(), backpackWrapper.getBackpack());
				}
				return backpackWrapper;
			}).orElse(IBackpackWrapper.Noop.INSTANCE);
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

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer) {
			return new BackpackContext.Block(BlockPos.of(packetBuffer.readLong()));
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			packetBuffer.writeLong(pos.asLong());
		}

		@Override
		public boolean canInteractWith(Player player) {
			return player.level().getBlockEntity(pos) instanceof BackpackBlockEntity && player.canReach(pos, 4.0D);
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
		private ItemStack clientChildBackpack = ItemStack.EMPTY;

		public BlockSubBackpack(BlockPos pos, int subBackpackSlotIndex, boolean saveAfterOpen) {
			super(pos);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
			this.saveAfterOpen = saveAfterOpen;
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
			return getParentBackpackWrapper(player).map(parent -> {
				ItemStack stack = parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex);
				if (stack.isEmpty() && player.level().isClientSide && !clientChildBackpack.isEmpty()) {
					stack = clientChildBackpack;
				}
				ItemStack childStack = stack;
				return getOrCreateBackpackWrapper(player, childStack,
						() -> childStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).orElse(IBackpackWrapper.Noop.INSTANCE));
			}).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public void setParentBackpackWrapper(IStorageWrapper parentWrapper) {
			this.parentWrapper = parentWrapper;
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer) {
			return new BackpackContext.BlockSubBackpack(BlockPos.of(packetBuffer.readLong()), packetBuffer.readInt(), packetBuffer.readBoolean());
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			super.addToBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
			packetBuffer.writeBoolean(saveAfterOpen);
		}

		@Override
		protected void writeClientContextData(FriendlyByteBuf packetBuffer, @Nullable Player player) {
			if (player == null) {
				super.writeClientContextData(packetBuffer, null);
				return;
			}
			packetBuffer.writeBoolean(true);
			packetBuffer.writeItem(getBackpackWrapper(player).getBackpack());
		}

		@Override
		protected void readClientContextData(FriendlyByteBuf packetBuffer) {
			if (packetBuffer.readBoolean()) {
				clientChildBackpack = packetBuffer.readItem();
			}
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
			if (parentWrapper != null && backpackWrapper != null && isCurrentSubBackpackStack(parentWrapper, subBackpackSlotIndex, backpackWrapper)) {
				persistSubBackpackStack(parentWrapper, subBackpackSlotIndex, backpackWrapper.getBackpack());
			}
		}

		@Override
		public void releaseBackpackWrapper() {
			super.releaseBackpackWrapper();
			if (parentWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
				linkedStorageBackpackWrapper.close();
			}
			parentWrapper = null;
		}
	}

	private static void persistSubBackpackStack(IStorageWrapper parentWrapper, int slotIndex, ItemStack subBackpack) {
		parentWrapper.getInventoryHandler().setStackInSlot(slotIndex, ItemStack.EMPTY);
		parentWrapper.getInventoryHandler().setStackInSlot(slotIndex, subBackpack);
		parentWrapper.getInventoryHandler().saveInventory();
	}

	private static boolean isCurrentSubBackpackStack(IStorageWrapper parentWrapper, int slotIndex, IBackpackWrapper subBackpackWrapper) {
		ItemStack currentStack = parentWrapper.getInventoryHandler().getStackInSlot(slotIndex);
		if (subBackpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
			return BackpackLinkedStorageResolver.hasSameEndpoint(linkedStorageBackpackWrapper.getBackpack(), currentStack);
		}
		return subBackpackWrapper.getBackpack() == currentStack;
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
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			packetBuffer.writeInt(otherPlayer.getId());
			packetBuffer.writeUtf(handlerName);
			packetBuffer.writeUtf(identifier);
			packetBuffer.writeInt(backpackSlotIndex);
		}

		@Override
		public boolean canInteractWith(Player player) {
			return otherPlayer.isAlive() && !otherPlayer.isRemoved() && player.canReach(otherPlayer, 4.0D);
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

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer, Level level) {
			int playerId = packetBuffer.readInt();
			Player otherPlayer = (Player) level.getEntity(playerId);

			return new BackpackContext.AnotherPlayer(packetBuffer.readUtf(), packetBuffer.readUtf(), packetBuffer.readInt(),
					Objects.requireNonNull(otherPlayer));
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

		public AnotherPlayerSubBackpack(Player otherPlayer, String handlerName, String identifier, int backpackSlotIndex, int subBackpackSlotIndex,
				boolean saveAfterOpen) {
			super(handlerName, identifier, backpackSlotIndex, otherPlayer);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
			this.saveAfterOpen = saveAfterOpen;
		}

		@Override
		public Optional<IStorageWrapper> getParentBackpackWrapper(Player player) {
			if (parentWrapper == null) {
				parentWrapper = new BackpackContext.AnotherPlayer(handlerName, identifier, backpackSlotIndex, otherPlayer).getBackpackWrapper(player);
			}
			return Optional.of(parentWrapper);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(Player player) {
			return getParentBackpackWrapper(player).map(parent -> {
				ItemStack stack = parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex);
				return getOrCreateBackpackWrapper(player, stack,
						() -> stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).orElse(IBackpackWrapper.Noop.INSTANCE));
			}).orElse(IBackpackWrapper.Noop.INSTANCE);
		}

		@Override
		public void setParentBackpackWrapper(IStorageWrapper parentWrapper) {
			this.parentWrapper = parentWrapper;
		}

		@Override
		public void addToBuffer(FriendlyByteBuf packetBuffer) {
			super.addToBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
			packetBuffer.writeBoolean(saveAfterOpen);
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
			return Component.literal(SUBBACKPACK_DISPLAY_NAME_PREFIX + super.getDisplayName(player).getString());
		}

		public static BackpackContext fromBuffer(FriendlyByteBuf packetBuffer, Level level) {
			int playerId = packetBuffer.readInt();
			Player otherPlayer = (Player) level.getEntity(playerId);

			return new BackpackContext.AnotherPlayerSubBackpack(Objects.requireNonNull(otherPlayer), packetBuffer.readUtf(), packetBuffer.readUtf(),
					packetBuffer.readInt(), packetBuffer.readInt(), packetBuffer.readBoolean());
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
				parentWrapper.getInventoryHandler().setStackInSlot(subBackpackSlotIndex,
						parentWrapper.getInventoryHandler().getStackInSlot(subBackpackSlotIndex));
				parentWrapper.getInventoryHandler().saveInventory();
			}
		}

		@Override
		public void releaseBackpackWrapper() {
			super.releaseBackpackWrapper();
			if (parentWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
				linkedStorageBackpackWrapper.close();
			}
			parentWrapper = null;
		}
	}

	private record LinkedStorageSnapshot(UUID groupId, long revision, CompoundTag contents, Component groupName, int inventorySlots, int upgradeSlots,
			int columnsTaken) {
	}
}
