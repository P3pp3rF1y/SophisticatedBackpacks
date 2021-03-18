package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.NoopBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.*;

public abstract class BackpackContext {
	private final ContainerType<BackpackContainer> containerType;

	protected BackpackContext(ContainerType<BackpackContainer> containerType) {
		this.containerType = containerType;
	}

	public abstract Optional<IBackpackWrapper> getParentBackpackWrapper(PlayerEntity player);

	public abstract boolean shouldLockBackpackSlot();

	public abstract IBackpackWrapper getBackpackWrapper(PlayerEntity player);

	public ContainerType<BackpackContainer> getContainerType() {
		return containerType;
	}

	public abstract int getBackpackSlotIndex();

	public abstract BackpackContext getSubBackpackContext(int subBackpackSlotIndex);

	public abstract BackpackContext getParentBackpackContext();

	public abstract void toBuffer(PacketBuffer packetBuffer);

	public abstract boolean canInteractWith(PlayerEntity player);

	public static class Item extends BackpackContext {
		protected final String handlerName;
		protected final int backpackSlotIndex;

		public Item(String handlerName, int backpackSlotIndex) {
			this(BACKPACK_ITEM_CONTAINER_TYPE.get(), handlerName, backpackSlotIndex);
		}

		public Item(ContainerType<BackpackContainer> containerType, String handlerName, int backpackSlotIndex) {
			super(containerType);
			this.handlerName = handlerName;
			this.backpackSlotIndex = backpackSlotIndex;
		}

		@Override
		public Optional<IBackpackWrapper> getParentBackpackWrapper(PlayerEntity player) {
			return Optional.empty();
		}

		@Override
		public boolean shouldLockBackpackSlot() {
			return PlayerInventoryProvider.getPlayerInventoryHandler(handlerName).map(PlayerInventoryHandler::isVisibleInGui).orElse(false);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(PlayerEntity player) {
			return PlayerInventoryProvider.getPlayerInventoryHandler(handlerName)
					.map(h -> h.getStackInSlot(player, backpackSlotIndex).getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).orElse(NoopBackpackWrapper.INSTANCE))
					.orElse(NoopBackpackWrapper.INSTANCE);
		}

		@Override
		public int getBackpackSlotIndex() {
			return backpackSlotIndex;
		}

		@Override
		public BackpackContext getSubBackpackContext(int subBackpackSlotIndex) {
			return new ItemSubBackpack(handlerName, backpackSlotIndex, subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return this;
		}

		public static BackpackContext fromBuffer(PacketBuffer packetBuffer) {
			return new BackpackContext.Item(packetBuffer.readString(), packetBuffer.readInt());
		}

		@Override
		public void toBuffer(PacketBuffer packetBuffer) {
			packetBuffer.writeString(handlerName);
			packetBuffer.writeInt(backpackSlotIndex);
		}

		@Override
		public boolean canInteractWith(PlayerEntity player) {
			return true;
		}
	}

	public static class ItemSubBackpack extends Item {
		private final int subBackpackSlotIndex;
		@Nullable
		private IBackpackWrapper parentWrapper;

		public ItemSubBackpack(String handlerName, int backpackSlotIndex, int subBackpackSlotIndex) {
			super(ITEM_SUBBACKPACK_CONTAINER_TYPE.get(), handlerName, backpackSlotIndex);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
		}

		@Override
		public Optional<IBackpackWrapper> getParentBackpackWrapper(PlayerEntity player) {
			if (parentWrapper == null) {
				parentWrapper = super.getBackpackWrapper(player);
			}
			return Optional.of(parentWrapper);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(PlayerEntity player) {
			return getParentBackpackWrapper(player).map(parent -> parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.orElse(NoopBackpackWrapper.INSTANCE)).orElse(NoopBackpackWrapper.INSTANCE);
		}

		public static BackpackContext fromBuffer(PacketBuffer packetBuffer) {
			return new BackpackContext.ItemSubBackpack(packetBuffer.readString(), packetBuffer.readInt(), packetBuffer.readInt());
		}

		@Override
		public void toBuffer(PacketBuffer packetBuffer) {
			super.toBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new BackpackContext.Item(handlerName, backpackSlotIndex);
		}
	}

	public static class Block extends BackpackContext {
		protected final BlockPos pos;

		public Block(BlockPos pos) {
			this(BACKPACK_BLOCK_CONTAINER_TYPE.get(), pos);
		}

		public Block(ContainerType<BackpackContainer> containerType, BlockPos pos) {
			super(containerType);
			this.pos = pos;
		}

		@Override
		public Optional<IBackpackWrapper> getParentBackpackWrapper(PlayerEntity player) {
			return Optional.empty();
		}

		@Override
		public boolean shouldLockBackpackSlot() {
			return false;
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(PlayerEntity player) {
			return WorldHelper.getTile(player.world, pos, BackpackTileEntity.class).map(BackpackTileEntity::getBackpackWrapper).orElse(NoopBackpackWrapper.INSTANCE);
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

		public static BackpackContext fromBuffer(PacketBuffer packetBuffer) {
			return new BackpackContext.Block(BlockPos.fromLong(packetBuffer.readLong()));
		}

		@Override
		public void toBuffer(PacketBuffer packetBuffer) {
			packetBuffer.writeLong(pos.toLong());
		}

		@Override
		public boolean canInteractWith(PlayerEntity player) {
			return player.world.getTileEntity(pos) instanceof BackpackTileEntity
					&& (player.getDistanceSq((double) pos.getX() + 0.5D, (double) pos.getY() + 0.5D, (double) pos.getZ() + 0.5D) <= 64.0D);
		}
	}

	public static class BlockSubBackpack extends Block {
		private final int subBackpackSlotIndex;
		@Nullable
		private IBackpackWrapper parentWrapper;

		public BlockSubBackpack(BlockPos pos, int subBackpackSlotIndex) {
			super(BLOCK_SUBBACKPACK_CONTAINER_TYPE.get(), pos);
			this.subBackpackSlotIndex = subBackpackSlotIndex;
		}

		@Override
		public Optional<IBackpackWrapper> getParentBackpackWrapper(PlayerEntity player) {
			if (parentWrapper == null) {
				parentWrapper = super.getBackpackWrapper(player);
			}
			return Optional.of(parentWrapper);
		}

		@Override
		public IBackpackWrapper getBackpackWrapper(PlayerEntity player) {
			return getParentBackpackWrapper(player).map(parent -> parent.getInventoryHandler().getStackInSlot(subBackpackSlotIndex).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.orElse(NoopBackpackWrapper.INSTANCE)).orElse(NoopBackpackWrapper.INSTANCE);
		}

		public static BackpackContext fromBuffer(PacketBuffer packetBuffer) {
			return new BackpackContext.BlockSubBackpack(BlockPos.fromLong(packetBuffer.readLong()), packetBuffer.readInt());
		}

		@Override
		public void toBuffer(PacketBuffer packetBuffer) {
			super.toBuffer(packetBuffer);
			packetBuffer.writeInt(subBackpackSlotIndex);
		}

		@Override
		public BackpackContext getParentBackpackContext() {
			return new BackpackContext.Block(pos);
		}
	}
}
