package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.core.UUIDUtil;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherHandler;

import java.util.UUID;

public record MobCatcherReleasePayload(UUID capturedMobId) implements CustomPacketPayload {
	public static final Type<MobCatcherReleasePayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("mob_catcher_release"));
	public static final StreamCodec<ByteBuf, MobCatcherReleasePayload> STREAM_CODEC = StreamCodec.composite(
			UUIDUtil.STREAM_CODEC,
			MobCatcherReleasePayload::capturedMobId,
			MobCatcherReleasePayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(MobCatcherReleasePayload payload, IPayloadContext context) {
		if (context.player() instanceof ServerPlayer serverPlayer) {
			MobCatcherHandler.release(serverPlayer, payload.capturedMobId);
		}
	}
}
