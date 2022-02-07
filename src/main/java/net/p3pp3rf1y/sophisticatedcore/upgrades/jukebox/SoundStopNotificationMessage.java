package net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.Supplier;

public class SoundStopNotificationMessage {
	private final UUID storageUuid;

	public SoundStopNotificationMessage(UUID storageUuid) {
		this.storageUuid = storageUuid;
	}

	public static void encode(SoundStopNotificationMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUUID(msg.storageUuid);
	}

	public static SoundStopNotificationMessage decode(FriendlyByteBuf packetBuffer) {
		return new SoundStopNotificationMessage(packetBuffer.readUUID());
	}

	public static void onMessage(SoundStopNotificationMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer sender, SoundStopNotificationMessage msg) {
		if (sender == null) {
			return;
		}
		ServerStorageSoundHandler.onSoundStopped((ServerLevel) sender.level, msg.storageUuid);
	}
}
