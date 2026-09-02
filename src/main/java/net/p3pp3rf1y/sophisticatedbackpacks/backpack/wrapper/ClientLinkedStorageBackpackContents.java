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
	private static final Map<UUID, Binding> BINDINGS = new HashMap<>();
	private static final Set<UUID> REQUESTED_GROUP_NAMES = new HashSet<>();
	private static final Set<UUID> UPDATED_GROUPS = new HashSet<>();

	private ClientLinkedStorageBackpackContents() {
	}

	public static Optional<ILinkedStorageContentsBinding> getBinding(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(snapshot -> BINDINGS.computeIfAbsent(groupId, Binding::new));
	}

	public static boolean install(UUID groupId, long revision, CompoundTag contents, Component name, int inventorySlots, int upgradeSlots, int columnsTaken) {
		Snapshot current = SNAPSHOTS.get(groupId);
		if (current != null && current.revision > revision) {
			return false;
		}
		Snapshot snapshot = new Snapshot(revision, contents.copy(), name, inventorySlots, upgradeSlots, columnsTaken);
		SNAPSHOTS.put(groupId, snapshot);
		Binding binding = BINDINGS.get(groupId);
		if (binding != null) {
			binding.snapshot = snapshot;
		}
		UPDATED_GROUPS.add(groupId);
		return true;
	}

	public static Optional<Component> getGroupName(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(snapshot -> snapshot.name);
	}

	public static Optional<Long> getRevision(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(snapshot -> snapshot.revision);
	}

	public static boolean requestGroupName(UUID groupId) {
		return REQUESTED_GROUP_NAMES.add(groupId);
	}

	public static Optional<StorageSize> getStorageSize(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(snapshot -> new StorageSize(snapshot.inventorySlots, snapshot.upgradeSlots));
	}

	public static Optional<Integer> getColumnsTaken(UUID groupId) {
		return Optional.ofNullable(SNAPSHOTS.get(groupId)).map(snapshot -> snapshot.columnsTaken);
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

	private static class Binding implements ILinkedStorageContentsBinding {
		private final UUID groupId;
		private Snapshot snapshot;
		private Binding(UUID groupId) {
			this.groupId = groupId;
			snapshot = SNAPSHOTS.get(groupId);
		}
		@Override
		public UUID groupId() {
			return groupId;
		}
		@Override
		public CompoundTag getContents() {
			return snapshot.contents;
		}
		@Override
		public void setContents(CompoundTag contents) {
			snapshot = snapshot.withContents(contents);
		}
		@Override
		public void markChanged() {
		}
		@Override
		public int getColumnsTaken() {
			return snapshot.columnsTaken;
		}
		@Override
		public void setColumnsTaken(int columnsTaken) {
			snapshot = snapshot.withColumnsTaken(columnsTaken);
		}
	}

	private record Snapshot(long revision, CompoundTag contents, Component name, int inventorySlots, int upgradeSlots, int columnsTaken) {
		private Snapshot withContents(CompoundTag contents) {
			return new Snapshot(revision, contents.copy(), name, inventorySlots, upgradeSlots, columnsTaken);
		}
		private Snapshot withColumnsTaken(int columnsTaken) {
			return new Snapshot(revision, contents, name, inventorySlots, upgradeSlots, columnsTaken);
		}
	}
}
