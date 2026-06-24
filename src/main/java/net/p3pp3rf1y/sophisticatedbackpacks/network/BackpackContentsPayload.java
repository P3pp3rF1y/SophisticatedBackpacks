package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.util.StreamCodecHelper;

import javax.annotation.Nullable;

import java.util.UUID;

public record BackpackContentsPayload(UUID backpackUuid, @Nullable CompoundTag backpackContents) implements CustomPacketPayload {
	public static final Type<BackpackContentsPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("backpack_contents"));
	public static final StreamCodec<ByteBuf, BackpackContentsPayload> STREAM_CODEC = StreamCodec.composite(UUIDUtil.STREAM_CODEC,
			BackpackContentsPayload::backpackUuid, StreamCodecHelper.ofNullable(ByteBufCodecs.COMPOUND_TAG), BackpackContentsPayload::backpackContents,
			BackpackContentsPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(BackpackContentsPayload payload, IPayloadContext context) {
		if (payload.backpackContents == null) {
			return;
		}

		BackpackStorage.get().setBackpackContents(payload.backpackUuid, payload.backpackContents);
		ClientStorageContentsTooltipBase.refreshContents();
	}
}
