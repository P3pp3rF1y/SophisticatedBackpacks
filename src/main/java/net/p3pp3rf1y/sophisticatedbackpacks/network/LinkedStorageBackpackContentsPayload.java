package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.core.UUIDUtil;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.ComponentSerialization;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerLevel;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageVirtualHost;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;

import java.util.UUID;

public record LinkedStorageBackpackContentsPayload(UUID groupId, long revision, ContainerContents contents, Component groupName, int inventorySlots,
		int upgradeSlots, int columnsTaken) implements CustomPacketPayload {
	public static final Type<LinkedStorageBackpackContentsPayload> TYPE = new Type<>(SophisticatedBackpacks.getIdentifier("linked_storage_backpack_contents"));
	private static final StreamCodec<ByteBuf, SnapshotProfile> SNAPSHOT_PROFILE_STREAM_CODEC = StreamCodec.composite(ByteBufCodecs.VAR_INT,
			SnapshotProfile::inventorySlots, ByteBufCodecs.VAR_INT, SnapshotProfile::upgradeSlots, ByteBufCodecs.VAR_INT, SnapshotProfile::columnsTaken,
			SnapshotProfile::new);
	public static final StreamCodec<RegistryFriendlyByteBuf, LinkedStorageBackpackContentsPayload> STREAM_CODEC = StreamCodec.composite(UUIDUtil.STREAM_CODEC,
			LinkedStorageBackpackContentsPayload::groupId, ByteBufCodecs.VAR_LONG, LinkedStorageBackpackContentsPayload::revision,
			ContainerContents.STREAM_CODEC, LinkedStorageBackpackContentsPayload::contents, ComponentSerialization.TRUSTED_CONTEXT_FREE_STREAM_CODEC,
			LinkedStorageBackpackContentsPayload::groupName, SNAPSHOT_PROFILE_STREAM_CODEC, LinkedStorageBackpackContentsPayload::snapshotProfile,
			LinkedStorageBackpackContentsPayload::fromStream);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	private SnapshotProfile snapshotProfile() {
		return new SnapshotProfile(inventorySlots, upgradeSlots, columnsTaken);
	}

	private static LinkedStorageBackpackContentsPayload fromStream(UUID groupId, long revision, ContainerContents contents, Component groupName,
			SnapshotProfile profile) {
		return new LinkedStorageBackpackContentsPayload(groupId, revision, contents, groupName, profile.inventorySlots(), profile.upgradeSlots(),
				profile.columnsTaken());
	}

	public static void handlePayload(LinkedStorageBackpackContentsPayload payload, IPayloadContext context) {
		if (ClientLinkedStorageBackpackContents.installSnapshot(payload.groupId, payload.revision, payload.contents, payload.groupName,
				new ClientLinkedStorageBackpackContents.StorageSize(payload.inventorySlots, payload.upgradeSlots), payload.columnsTaken)) {
			ClientStorageContentsTooltipBase.refreshContents();
		}
	}

	public static LinkedStorageBackpackContentsPayload createSnapshot(ServerLevel level, UUID groupId) {
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		ILinkedStorageVirtualHost virtualHost = manager.resolveVirtualHost(groupId)
				.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack host for group " + groupId));
		if (!(virtualHost instanceof IBackpackWrapper host)) {
			throw new IllegalStateException("Linked storage group " + groupId + " does not have a backpack host");
		}
		ILinkedStorageContentsBinding contents = manager.resolveContents(groupId)
				.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack contents for group " + groupId));
		return new LinkedStorageBackpackContentsPayload(groupId, manager.getRevision(groupId), contents.contents().copy(), host.getDisplayName(),
				getBaseInventorySlots(host), host.getUpgradeHandler().size(), host.getColumnsTaken());
	}

	private static int getBaseInventorySlots(IBackpackWrapper host) {
		return host.getInventoryHandler().size() + host.getColumnsTaken() * host.getNumberOfSlotRows();
	}

	private record SnapshotProfile(int inventorySlots, int upgradeSlots, int columnsTaken) {
	}
}
