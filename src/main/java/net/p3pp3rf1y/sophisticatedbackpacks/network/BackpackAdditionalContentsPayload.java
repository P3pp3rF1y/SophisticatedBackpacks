package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;

import java.util.UUID;

public record BackpackAdditionalContentsPayload(UUID backpackUuid, CompoundTag additionalContents) implements CustomPacketPayload {
	public static final Type<BackpackAdditionalContentsPayload> TYPE = new Type<>(SophisticatedBackpacks.getIdentifier("backpack_additional_contents"));
	public static final StreamCodec<RegistryFriendlyByteBuf, BackpackAdditionalContentsPayload> STREAM_CODEC = StreamCodec.composite(UUIDUtil.STREAM_CODEC,
			BackpackAdditionalContentsPayload::backpackUuid, ByteBufCodecs.COMPOUND_TAG, BackpackAdditionalContentsPayload::additionalContents,
			BackpackAdditionalContentsPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(BackpackAdditionalContentsPayload payload, IPayloadContext context) {
		BackpackStorage.get().setAdditionalBackpackContents(payload.backpackUuid, payload.additionalContents);
		ClientStorageContentsTooltipBase.refreshContents();
	}
}
