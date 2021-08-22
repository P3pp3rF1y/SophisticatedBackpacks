package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.fml.network.NetworkEvent;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.Supplier;

public class SoundStopNotificationMessage {
	private final UUID backpackUuid;

	public SoundStopNotificationMessage(UUID backpackUuid) {
		this.backpackUuid = backpackUuid;
	}

	public static void encode(SoundStopNotificationMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
	}

	public static SoundStopNotificationMessage decode(PacketBuffer packetBuffer) {
		return new SoundStopNotificationMessage(packetBuffer.readUUID());
	}

	public static void onMessage(SoundStopNotificationMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity sender, SoundStopNotificationMessage msg) {
		if (sender == null) {
			return;
		}
		ServerBackpackSoundHandler.onSoundStopped((ServerWorld) sender.level, msg.backpackUuid);
	}
}
