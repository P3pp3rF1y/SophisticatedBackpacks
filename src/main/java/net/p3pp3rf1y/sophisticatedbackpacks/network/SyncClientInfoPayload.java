package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedcore.util.StreamCodecHelper;

import javax.annotation.Nullable;

public record SyncClientInfoPayload(int slotIndex, @Nullable CompoundTag renderInfoNbt,
									int columnsTaken) implements CustomPacketPayload {
	public static final Type<SyncClientInfoPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("sync_client_info"));
	public static final StreamCodec<ByteBuf, SyncClientInfoPayload> STREAM_CODEC = StreamCodec.composite(
			ByteBufCodecs.INT,
			SyncClientInfoPayload::slotIndex,
			StreamCodecHelper.ofNullable(ByteBufCodecs.COMPOUND_TAG),
			SyncClientInfoPayload::renderInfoNbt,
			ByteBufCodecs.INT,
			SyncClientInfoPayload::columnsTaken,
			SyncClientInfoPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(SyncClientInfoPayload payload, IPayloadContext context) {
		Player player = context.player();
		if (payload.renderInfoNbt == null || !(player.containerMenu instanceof BackpackContainer backpackContainer)) {
			return;
		}
		if (payload.slotIndex >= 0) {
			ItemStack backpack = player.getInventory().items.get(payload.slotIndex);
			IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(backpack);
			backpackWrapper.getRenderInfo().deserializeFrom(payload.renderInfoNbt);
			backpackWrapper.setColumnsTaken(payload.columnsTaken, false);
		}
		if (backpackContainer.canApplyClientInfo(payload.slotIndex)) {
			backpackContainer.syncClientInfo(payload.renderInfoNbt, payload.columnsTaken);
		}
	}
}
