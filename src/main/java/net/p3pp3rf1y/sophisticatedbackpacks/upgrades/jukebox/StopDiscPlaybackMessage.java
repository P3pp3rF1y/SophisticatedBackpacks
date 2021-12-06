package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraftforge.network.NetworkEvent;

import java.util.UUID;
import java.util.function.Supplier;

public class StopDiscPlaybackMessage {
	private final UUID backpackUuid;

	public StopDiscPlaybackMessage(UUID backpackUuid) {
		this.backpackUuid = backpackUuid;
	}

	public static void encode(StopDiscPlaybackMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
	}

	public static StopDiscPlaybackMessage decode(FriendlyByteBuf packetBuffer) {
		return new StopDiscPlaybackMessage(packetBuffer.readUUID());
	}

	public static void onMessage(StopDiscPlaybackMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(StopDiscPlaybackMessage msg) {
		BackpackSoundHandler.stopBackpackSound(msg.backpackUuid);
	}
}
