package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.core.UUIDUtil;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.PacketDistributor;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.network.ItemStackReference;

import java.util.UUID;

public record RequestBackpackInventoryContentsPayload(UUID backpackUuid, ItemStackReference itemReference) implements CustomPacketPayload {
	public static final Type<RequestBackpackInventoryContentsPayload> TYPE = new Type<>(
			SophisticatedBackpacks.getIdentifier("request_backpack_inventory_contents"));
	public static final StreamCodec<ByteBuf, RequestBackpackInventoryContentsPayload> STREAM_CODEC = StreamCodec.composite(UUIDUtil.STREAM_CODEC,
			RequestBackpackInventoryContentsPayload::backpackUuid, ItemStackReference.STREAM_CODEC, RequestBackpackInventoryContentsPayload::itemReference,
			RequestBackpackInventoryContentsPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(RequestBackpackInventoryContentsPayload payload, IPayloadContext context) {
		if (!(context.player() instanceof ServerPlayer player)) {
			return;
		}
		ItemStackReference reference = payload.itemReference;
		ItemStack stack = reference.resolve(player);
		if (reference.containerId() == ItemStackReference.EQUIPMENT_HANDLER) {
			stack = PlayerInventoryProvider.get().getPlayerInventoryHandler(reference.handlerName())
					.filter(handler -> handler.getIdentifiers(player).contains(reference.identifier()) && reference.slot() >= 0
							&& reference.slot() < handler.getSlotCount(player, reference.identifier()))
					.map(handler -> handler.getStackInSlot(player, reference.identifier(), reference.slot())).orElse(ItemStack.EMPTY);
		}
		if (!(stack.getItem() instanceof BackpackItem) || stack.has(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT)
				|| !payload.backpackUuid.equals(stack.get(ModCoreDataComponents.STORAGE_UUID))) {
			return;
		}
		BackpackStorage.get().getBackpackContents(payload.backpackUuid)
				.ifPresent(contents -> PacketDistributor.sendToPlayer(player, new BackpackContentsPayload(payload.backpackUuid, contents)));
	}
}
