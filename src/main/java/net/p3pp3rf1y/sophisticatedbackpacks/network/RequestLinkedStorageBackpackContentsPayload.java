package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.core.UUIDUtil;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.PacketDistributor;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import java.util.UUID;

public record RequestLinkedStorageBackpackContentsPayload(UUID groupId, long knownRevision) implements CustomPacketPayload {
	public static final Type<RequestLinkedStorageBackpackContentsPayload> TYPE = new Type<>(
			SophisticatedBackpacks.getRL("request_linked_storage_backpack_contents"));
	public static final StreamCodec<ByteBuf, RequestLinkedStorageBackpackContentsPayload> STREAM_CODEC = StreamCodec.composite(UUIDUtil.STREAM_CODEC,
			RequestLinkedStorageBackpackContentsPayload::groupId, ByteBufCodecs.VAR_LONG, RequestLinkedStorageBackpackContentsPayload::knownRevision,
			RequestLinkedStorageBackpackContentsPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(RequestLinkedStorageBackpackContentsPayload payload, IPayloadContext context) {
		if (!(context.player() instanceof ServerPlayer player)) {
			return;
		}
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(player.level()).manager();
		if (!hasGroupEndpoint(player, manager, payload.groupId)) {
			return;
		}
		if (manager.getRevision(payload.groupId) != payload.knownRevision) {
			PacketDistributor.sendToPlayer(player, LinkedStorageBackpackContentsPayload.createSnapshot(player.level(), payload.groupId));
		}
	}

	private static boolean hasGroupEndpoint(ServerPlayer player, LinkedStorageGroupManager manager, UUID groupId) {
		return PlayerInventoryProvider.get().runOnBackpacks(player, (stack, inventoryName, identifier, slot) -> isGroupEndpoint(stack, manager, groupId))
				|| hasGroupEndpointInOpenMenu(player, manager, groupId);
	}

	private static boolean hasGroupEndpointInOpenMenu(ServerPlayer player, LinkedStorageGroupManager manager, UUID groupId) {
		if (!(player.containerMenu instanceof BackpackContainer backpackMenu)) {
			return false;
		}
		InventoryHandler inventory = backpackMenu.getStorageWrapper().getInventoryHandler();
		for (int slot = 0; slot < inventory.size(); slot++) {
			if (isGroupEndpoint(inventory.getStackInSlot(slot), manager, groupId)) {
				return true;
			}
		}
		return false;
	}

	private static boolean isGroupEndpoint(ItemStack stack, LinkedStorageGroupManager manager, UUID groupId) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return false;
		}
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		return endpoint.groupId().equals(groupId) && manager.isEndpointMember(groupId, endpoint.endpointId());
	}
}
