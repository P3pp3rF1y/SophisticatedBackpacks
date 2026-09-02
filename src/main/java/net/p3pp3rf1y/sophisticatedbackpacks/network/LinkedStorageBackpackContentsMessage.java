package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;

import java.util.UUID;
import java.util.function.Supplier;

public record LinkedStorageBackpackContentsMessage(UUID groupId, long revision, CompoundTag contents, Component name, int inventorySlots, int upgradeSlots,
		int columnsTaken) {
	public static void encode(LinkedStorageBackpackContentsMessage message, FriendlyByteBuf buffer) {
		buffer.writeUUID(message.groupId);
		buffer.writeVarLong(message.revision);
		buffer.writeNbt(message.contents);
		buffer.writeComponent(message.name);
		buffer.writeVarInt(message.inventorySlots);
		buffer.writeVarInt(message.upgradeSlots);
		buffer.writeVarInt(message.columnsTaken);
	}
	public static LinkedStorageBackpackContentsMessage decode(FriendlyByteBuf buffer) {
		return new LinkedStorageBackpackContentsMessage(buffer.readUUID(), buffer.readVarLong(), buffer.readAnySizeNbt(), buffer.readComponent(),
				buffer.readVarInt(), buffer.readVarInt(), buffer.readVarInt());
	}
	public static void onMessage(LinkedStorageBackpackContentsMessage message, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> {
			if (Minecraft.getInstance().player != null && message.contents != null) {
				ClientLinkedStorageBackpackContents.install(message.groupId, message.revision, message.contents, message.name, message.inventorySlots,
						message.upgradeSlots, message.columnsTaken);
				ClientStorageContentsTooltipBase.refreshContents();
			}
		});
		context.setPacketHandled(true);
	}
	public static LinkedStorageBackpackContentsMessage createSnapshot(ServerLevel level, UUID groupId) {
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		IBackpackWrapper host = manager.resolveVirtualHost(groupId).filter(IBackpackWrapper.class::isInstance).map(IBackpackWrapper.class::cast).orElseThrow();
		return new LinkedStorageBackpackContentsMessage(groupId, manager.getRevision(groupId),
				manager.resolveContents(groupId).orElseThrow().getContents().copy(), host.getDisplayName(),
				host.getInventoryHandler().getSlots() + host.getColumnsTaken() * host.getNumberOfSlotRows(), host.getUpgradeHandler().getSlots(),
				host.getColumnsTaken());
	}
}
