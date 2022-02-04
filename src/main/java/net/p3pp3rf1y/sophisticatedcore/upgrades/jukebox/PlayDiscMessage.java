package net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox;

import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.RecordItem;
import net.minecraftforge.network.NetworkEvent;

import java.util.UUID;
import java.util.function.Supplier;

public class PlayDiscMessage {
	private final boolean blockStorage;
	private final UUID storageUuid;
	private final int musicDiscItemId;
	private int entityId;
	private BlockPos pos;

	public PlayDiscMessage(UUID storageUuid, int musicDiscItemId, BlockPos pos) {
		blockStorage = true;
		this.storageUuid = storageUuid;
		this.musicDiscItemId = musicDiscItemId;
		this.pos = pos;
	}

	public PlayDiscMessage(UUID storageUuid, int musicDiscItemId, int entityId) {
		blockStorage = false;
		this.storageUuid = storageUuid;
		this.musicDiscItemId = musicDiscItemId;
		this.entityId = entityId;
	}

	public static void encode(PlayDiscMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeBoolean(msg.blockStorage);
		packetBuffer.writeUUID(msg.storageUuid);
		packetBuffer.writeInt(msg.musicDiscItemId);
		if (msg.blockStorage) {
			packetBuffer.writeBlockPos(msg.pos);
		} else {
			packetBuffer.writeInt(msg.entityId);
		}
	}

	public static PlayDiscMessage decode(FriendlyByteBuf packetBuffer) {
		if (packetBuffer.readBoolean()) {
			return new PlayDiscMessage(packetBuffer.readUUID(), packetBuffer.readInt(), packetBuffer.readBlockPos());
		}
		return new PlayDiscMessage(packetBuffer.readUUID(), packetBuffer.readInt(), packetBuffer.readInt());
	}

	public static void onMessage(PlayDiscMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(PlayDiscMessage msg) {
		Item discItem = Item.byId(msg.musicDiscItemId);
		if (!(discItem instanceof RecordItem)) {
			return;
		}
		SoundEvent soundEvent = ((RecordItem) discItem).getSound();
		UUID storageUuid = msg.storageUuid;
		if (msg.blockStorage) {
			StorageSoundHandler.playStorageSound(soundEvent, storageUuid, msg.pos);
		} else {
			StorageSoundHandler.playStorageSound(soundEvent, storageUuid, msg.entityId);
		}
	}
}
