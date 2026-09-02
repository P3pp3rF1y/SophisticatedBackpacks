package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;

import java.util.UUID;
import java.util.function.Supplier;

public record RequestLinkedStorageBackpackContentsMessage(UUID groupId, long knownRevision) {
	public static void encode(RequestLinkedStorageBackpackContentsMessage message, FriendlyByteBuf buffer) {
		buffer.writeUUID(message.groupId);
		buffer.writeVarLong(message.knownRevision);
	}
	public static RequestLinkedStorageBackpackContentsMessage decode(FriendlyByteBuf buffer) {
		return new RequestLinkedStorageBackpackContentsMessage(buffer.readUUID(), buffer.readVarLong());
	}
	public static void onMessage(RequestLinkedStorageBackpackContentsMessage message, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handle(context.getSender(), message));
		context.setPacketHandled(true);
	}
	private static void handle(ServerPlayer player, RequestLinkedStorageBackpackContentsMessage message) {
		if (player == null) {
			return;
		}
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(player.serverLevel()).manager();
		if (!ownsEndpoint(player, manager, message.groupId) || manager.getRevision(message.groupId) == message.knownRevision) {
			return;
		}
		SBPPacketHandler.INSTANCE.sendToClient(player, LinkedStorageBackpackContentsMessage.createSnapshot(player.serverLevel(), message.groupId));
	}
	private static boolean ownsEndpoint(ServerPlayer player, LinkedStorageGroupManager manager, UUID groupId) {
		return player.getInventory().items.stream().anyMatch(stack -> isEndpoint(stack, manager, groupId))
				|| player.getInventory().offhand.stream().anyMatch(stack -> isEndpoint(stack, manager, groupId));
	}
	private static boolean isEndpoint(net.minecraft.world.item.ItemStack stack, LinkedStorageGroupManager manager, UUID groupId) {
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		return endpoint != null && endpoint.groupId().equals(groupId) && manager.isEndpointMember(groupId, endpoint.endpointId());
	}
}
