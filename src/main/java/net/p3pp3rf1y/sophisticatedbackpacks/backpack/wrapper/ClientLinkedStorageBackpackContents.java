package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public final class ClientLinkedStorageBackpackContents {
	private static final Map<UUID, Snapshot> SNAPSHOTS = new HashMap<>();
	private static final Map<UUID, ClientBinding> BINDINGS = new HashMap<>();
	private static final Set<UUID> REQUESTED_GROUP_NAMES = new HashSet<>();
	private static final Set<UUID> UPDATED_GROUPS = new HashSet<>();

	private ClientLinkedStorageBackpackContents() {
	}

	public static Optional<ILinkedStorageContentsBinding> getBinding(UUID groupId) {
		if (!SNAPSHOTS.containsKey(groupId)) {
			return Optional.empty();
		}
		return Optional.of(BINDINGS.computeIfAbsent(groupId, ClientBinding::new));
	}

	private static final class ClientBinding implements ILinkedStorageContentsBinding {
		private final UUID groupId;
		private Snapshot snapshot;

		private ClientBinding(UUID groupId) {
			this.groupId = groupId;
			snapshot = SNAPSHOTS.get(groupId);
		}

		private void update(Snapshot snapshot) {
			this.snapshot = snapshot;
		}

		@Override
		public UUID groupId() {
			return groupId;
		}

		@Override
		public CompoundTag getContents(UUID storageId) {
			return snapshot.contents();
		}

		@Override
		public void setContents(UUID storageId, CompoundTag contents) {
			installSnapshot(groupId, snapshot.revision(), contents, snapshot.groupName(), snapshot.storageSize(), snapshot.columnsTaken());
		}

		@Override
		public void markChanged() {
		}

		@Override
		public int getColumnsTaken() {
			return snapshot.columnsTaken();
		}

		@Override
		public void setColumnsTaken(int columnsTaken) {
			installSnapshot(groupId, snapshot.revision(), snapshot.contents(), snapshot.groupName(), snapshot.storageSize(), columnsTaken);
		}
	}

	public static boolean installSnapshot(UUID groupId, long revision, CompoundTag contents, Component groupName, StorageSize storageSize, int columnsTaken) {
		Snapshot current = SNAPSHOTS.get(groupId);
		if (current != null && revision < current.revision()) {
			return false;
		}
		Snapshot snapshot = new Snapshot(contents.copy(), revision, groupName, storageSize, columnsTaken);
		SNAPSHOTS.put(groupId, snapshot);
		ClientBinding binding = BINDINGS.get(groupId);
		if (binding != null) {
			binding.update(snapshot);
		}
		UPDATED_GROUPS.add(groupId);
		return true;
	}

	public static Optional<Component> getGroupName(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(Snapshot::groupName);
	}

	public static Optional<Long> getRevision(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(Snapshot::revision);
	}

	public static boolean requestGroupName(UUID groupId) {
		return REQUESTED_GROUP_NAMES.add(groupId);
	}

	public static Optional<StorageSize> getStorageSize(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(Snapshot::storageSize);
	}

	public static Optional<Integer> getColumnsTaken(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(Snapshot::columnsTaken);
	}

	public static boolean removeUpdatedGroup(UUID groupId) {
		return UPDATED_GROUPS.remove(groupId);
	}

	public static void clear() {
		SNAPSHOTS.clear();
		BINDINGS.clear();
		REQUESTED_GROUP_NAMES.clear();
		UPDATED_GROUPS.clear();
	}

	public record StorageSize(int inventorySlots, int upgradeSlots) {
	}

	private record Snapshot(CompoundTag contents, long revision, Component groupName, StorageSize storageSize, int columnsTaken) {
	}
}
