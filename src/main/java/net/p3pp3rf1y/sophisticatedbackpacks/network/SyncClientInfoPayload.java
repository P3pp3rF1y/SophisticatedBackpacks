package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.util.StreamCodecHelper;
import org.jspecify.annotations.Nullable;

public record SyncClientInfoPayload(int slotIndex, @Nullable RenderData data, int columnsTaken) implements CustomPacketPayload {
	public static final Type<SyncClientInfoPayload> TYPE = new Type<>(SophisticatedBackpacks.getIdentifier("sync_client_info"));
	public static final StreamCodec<RegistryFriendlyByteBuf, SyncClientInfoPayload> STREAM_CODEC = StreamCodec.composite(ByteBufCodecs.INT,
			SyncClientInfoPayload::slotIndex, StreamCodecHelper.ofNullable(RenderData.STREAM_CODEC), SyncClientInfoPayload::data, ByteBufCodecs.INT,
			SyncClientInfoPayload::columnsTaken, SyncClientInfoPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(SyncClientInfoPayload payload, IPayloadContext context) {
		Player player = context.player();
		if (payload.data == null) {
			return;
		}
		if (payload.slotIndex >= 0) {
			ItemStack backpack = player.getInventory().getItem(payload.slotIndex);
			backpack.set(ModCoreDataComponents.RENDER_DATA, payload.data.copy());
			backpack.set(ModDataComponents.COLUMNS_TAKEN, payload.columnsTaken);
		}
		if (player.containerMenu instanceof BackpackContainer backpackContainer && backpackContainer.canApplyClientInfo(payload.slotIndex)) {
			backpackContainer.syncClientInfo(payload.data, payload.columnsTaken);
		}
	}
}
