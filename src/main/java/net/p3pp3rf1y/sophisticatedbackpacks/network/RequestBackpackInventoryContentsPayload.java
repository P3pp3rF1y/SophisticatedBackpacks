package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.core.UUIDUtil;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.neoforge.network.PacketDistributor;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

import java.util.UUID;

public record RequestBackpackInventoryContentsPayload(UUID backpackUuid) implements CustomPacketPayload {
	public static final Type<RequestBackpackInventoryContentsPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("request_backpack_inventory_contents"));
	public static final StreamCodec<ByteBuf, RequestBackpackInventoryContentsPayload> STREAM_CODEC = StreamCodec.composite(
			UUIDUtil.STREAM_CODEC,
			RequestBackpackInventoryContentsPayload::backpackUuid,
			RequestBackpackInventoryContentsPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(RequestBackpackInventoryContentsPayload payload, IPayloadContext context) {
		ContainerContents backpackContents = BackpackStorage.get().getOrCreateBackpackContents(payload.backpackUuid);
		if (context.player() instanceof ServerPlayer serverPlayer) {
			PacketDistributor.sendToPlayer(serverPlayer, new BackpackContentsPayload(payload.backpackUuid, backpackContents));
		}
	}
}
