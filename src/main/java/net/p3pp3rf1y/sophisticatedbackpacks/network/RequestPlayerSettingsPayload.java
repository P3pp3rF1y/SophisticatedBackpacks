package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.neoforged.neoforge.network.PacketDistributor;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.network.SyncPlayerSettingsPayload;
import net.p3pp3rf1y.sophisticatedcore.settings.main.PlayerMainSettingsSavedData;
import net.p3pp3rf1y.sophisticatedcore.util.StreamCodecHelper;

public record RequestPlayerSettingsPayload() implements CustomPacketPayload {
	public static final Type<RequestPlayerSettingsPayload> TYPE = new Type<>(SophisticatedBackpacks.getIdentifier("request_player_settings"));
	public static final StreamCodec<ByteBuf, RequestPlayerSettingsPayload> STREAM_CODEC = StreamCodecHelper.singleton(RequestPlayerSettingsPayload::new);

	public static void handlePayload(@SuppressWarnings("unused") RequestPlayerSettingsPayload payload, IPayloadContext context) {
		Player player = context.player();
		String name = SophisticatedBackpacks.MOD_ID;
		if (player instanceof ServerPlayer serverPlayer) {
			PacketDistributor.sendToPlayer(serverPlayer, new SyncPlayerSettingsPayload(name, PlayerMainSettingsSavedData.get().get(player.getUUID(), name)));
		}
	}

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}
}
