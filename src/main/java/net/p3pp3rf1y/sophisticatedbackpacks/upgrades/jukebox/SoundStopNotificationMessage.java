package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.Supplier;

public class SoundStopNotificationMessage {
	private final UUID backpackUuid;

	public SoundStopNotificationMessage(UUID backpackUuid) {
		this.backpackUuid = backpackUuid;
	}

	public static void encode(SoundStopNotificationMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
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
		ServerBackpackSoundHandler.onSoundStopped((ServerLevel) sender.level, msg.backpackUuid);
	}
}
