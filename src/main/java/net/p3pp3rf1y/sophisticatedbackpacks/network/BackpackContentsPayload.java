package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.core.UUIDUtil;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import org.jspecify.annotations.Nullable;

import java.util.UUID;

public record BackpackContentsPayload(UUID backpackUuid, @Nullable ContainerContents backpackContents) implements CustomPacketPayload {
	public static final Type<BackpackContentsPayload> TYPE = new Type<>(SophisticatedBackpacks.getIdentifier("backpack_contents"));
	public static final StreamCodec<RegistryFriendlyByteBuf, BackpackContentsPayload> STREAM_CODEC = StreamCodec.composite(UUIDUtil.STREAM_CODEC,
			BackpackContentsPayload::backpackUuid, ContainerContents.STREAM_CODEC, BackpackContentsPayload::backpackContents, BackpackContentsPayload::new);

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
