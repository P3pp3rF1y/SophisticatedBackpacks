package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryInteractionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

public record InventoryInteractionPayload(BlockPos pos, Direction face) implements CustomPacketPayload {
	public static final Type<InventoryInteractionPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("inventory_interaction"));
	public static final StreamCodec<ByteBuf, InventoryInteractionPayload> STREAM_CODEC = StreamCodec.composite(BlockPos.STREAM_CODEC,
			InventoryInteractionPayload::pos, Direction.STREAM_CODEC, InventoryInteractionPayload::face, InventoryInteractionPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(InventoryInteractionPayload payload, IPayloadContext context) {
		Player player = context.player();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryName, identifier, slot) -> {
			InventoryInteractionHelper.tryInventoryInteraction(payload.pos, player.level(), backpack, payload.face, player);
			player.swing(InteractionHand.MAIN_HAND, true);
			return true;
		});
	}
}
