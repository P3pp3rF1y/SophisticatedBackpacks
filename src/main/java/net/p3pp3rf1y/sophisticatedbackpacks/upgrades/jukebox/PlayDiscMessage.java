package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.item.Item;
import net.minecraft.item.MusicDiscItem;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.SoundEvent;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fml.network.NetworkEvent;

import java.util.UUID;
import java.util.function.Supplier;

public class PlayDiscMessage {
	private final boolean blockBackpack;
	private final UUID backpackUuid;
	private final int musicDiscItemId;
	private int entityId;
	private BlockPos pos;

	public PlayDiscMessage(UUID backpackUuid, int musicDiscItemId, BlockPos pos) {
		blockBackpack = true;
		this.backpackUuid = backpackUuid;
		this.musicDiscItemId = musicDiscItemId;
		this.pos = pos;
	}

	public PlayDiscMessage(UUID backpackUuid, int musicDiscItemId, int entityId) {
		blockBackpack = false;
		this.backpackUuid = backpackUuid;
		this.musicDiscItemId = musicDiscItemId;
		this.entityId = entityId;
	}

	public static void encode(PlayDiscMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeBoolean(msg.blockBackpack);
		packetBuffer.writeUUID(msg.backpackUuid);
		packetBuffer.writeInt(msg.musicDiscItemId);
		if (msg.blockBackpack) {
			packetBuffer.writeBlockPos(msg.pos);
		} else {
			packetBuffer.writeInt(msg.entityId);
		}
	}

	public static PlayDiscMessage decode(PacketBuffer packetBuffer) {
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
		if (!(discItem instanceof MusicDiscItem)) {
			return;
		}
		SoundEvent soundEvent = ((MusicDiscItem) discItem).getSound();
		UUID backpackUuid = msg.backpackUuid;
		if (msg.blockBackpack) {
			BackpackSoundHandler.playBackpackSound(soundEvent, backpackUuid, msg.pos);
		} else {
			BackpackSoundHandler.playBackpackSound(soundEvent, backpackUuid, msg.entityId);
		}
	}
}
