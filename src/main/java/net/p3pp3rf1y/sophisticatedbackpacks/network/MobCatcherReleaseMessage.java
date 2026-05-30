package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherHandler;

import java.util.UUID;
import java.util.function.Supplier;

public record MobCatcherReleaseMessage(UUID capturedMobId) {
	public static void encode(MobCatcherReleaseMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUUID(msg.capturedMobId);
	}

	public static MobCatcherReleaseMessage decode(FriendlyByteBuf packetBuffer) {
		return new MobCatcherReleaseMessage(packetBuffer.readUUID());
	}

	public static void onMessage(MobCatcherReleaseMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> {
			ServerPlayer serverPlayer = context.getSender();
			if (serverPlayer != null) {
				MobCatcherHandler.release(serverPlayer, msg.capturedMobId);
			}
		});
		context.setPacketHandled(true);
	}
}
