package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import net.minecraft.world.level.storage.SavedDataStorage;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.neoforged.neoforge.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.util.CodecHelper;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

//TODO after 1.22 remove support for legacy UUID deserialization via strings
public class BackpackStorage extends SavedData {
	private static final SavedDataType<BackpackStorage> TYPE = new SavedDataType<>(Identifier.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "backpack_storage"), BackpackStorage::new,
			RecordCodecBuilder.create(
					builder -> builder.group(
							Codec.unboundedMap(
									CodecHelper.STRING_ENCODED_UUID,
									ContainerContents.CODEC
							).fieldOf("backpackContents").forGetter(storage -> storage.backpackContents),
							Codec.unboundedMap(
									CodecHelper.STRING_ENCODED_UUID, AccessLogRecord.CODEC
							).fieldOf("accessLogRecords").forGetter(storage -> storage.accessLogRecords)
					).apply(builder, BackpackStorage::new)
			));

	private final Map<UUID, ContainerContents> backpackContents = new HashMap<>();
	private static final BackpackStorage clientStorageCopy = new BackpackStorage();
	private final Map<UUID, AccessLogRecord> accessLogRecords = new HashMap<>();

	private BackpackStorage(Map<UUID, ContainerContents> backpackContents, Map<UUID, AccessLogRecord> accessLogRecords) {
		this.accessLogRecords.putAll(accessLogRecords);
		backpackContents.forEach(
				(uuid, contents) -> {
					if (isPlayerBackpackOrNotEmpty(this, uuid, contents)) {
						this.backpackContents.put(uuid, contents);
					}
				}
		);
	}

	private BackpackStorage() {
	}

	public static BackpackStorage get() {
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
			if (server != null) {
				ServerLevel overworld = server.getLevel(Level.OVERWORLD);
				//noinspection ConstantConditions - by this time overworld is loaded
				SavedDataStorage storage = overworld.getDataStorage();
				return storage.computeIfAbsent(TYPE);
			}
		}
		return clientStorageCopy;
	}

	private static boolean isPlayerBackpackOrNotEmpty(BackpackStorage storage, UUID backpackUuid, ContainerContents contents) {
		if (storage.accessLogRecords.containsKey(backpackUuid)) {
			return true;
		}
		return !contents.inventory().stacks().isEmpty();
	}

	public ContainerContents getOrCreateBackpackContents(UUID backpackUuid) {
		return backpackContents.computeIfAbsent(backpackUuid, uuid -> {
			setDirty();
			return new ContainerContents();
		});
	}

	public void putAccessLog(AccessLogRecord alr) {
		accessLogRecords.put(alr.backpackUuid(), alr);
		setDirty();
	}

	public void removeBackpackContents(UUID backpackUuid) {
		backpackContents.remove(backpackUuid);
		setDirty();
	}

	public void setBackpackContents(UUID backpackUuid, ContainerContents contents) {
		if (!backpackContents.containsKey(backpackUuid)) {
			backpackContents.put(backpackUuid, contents);
			updatedBackpackSettingsFlags.add(backpackUuid);
		} else {
			ContainerContents currentContents = backpackContents.get(backpackUuid);
			ContainerContents.SettingsData previousSettings = currentContents.settings().copy();
			currentContents.reloadFrom(contents);
			if (!currentContents.settings().equals(previousSettings)) {
				updatedBackpackSettingsFlags.add(backpackUuid);
			}
			setDirty();
		}
	}

	public Map<UUID, AccessLogRecord> getAccessLogs() {
		return accessLogRecords;
	}

	public int removeNonPlayerBackpackContents(boolean onlyWithEmptyInventory) {
		AtomicInteger numberRemoved = new AtomicInteger(0);
		backpackContents.entrySet().removeIf(entry -> {
			if (!accessLogRecords.containsKey(entry.getKey()) && (!onlyWithEmptyInventory || entry.getValue().inventory().stacks().isEmpty())) {
				numberRemoved.incrementAndGet();
				return true;
			}
			return false;
		});
		if (numberRemoved.get() > 0) {
			setDirty();
		}
		return numberRemoved.get();
	}

	private final Set<UUID> updatedBackpackSettingsFlags = new HashSet<>();

	public boolean removeUpdatedBackpackSettingsFlag(UUID backpackUuid) {
		return updatedBackpackSettingsFlags.remove(backpackUuid);
	}

	public static void onClientWorldLoad(LevelEvent.Load evt) {
		if (evt.getLevel().isClientSide()) {
			clientStorageCopy.backpackContents.clear();
			clientStorageCopy.accessLogRecords.clear();
		}
	}
}
