package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import net.minecraft.world.level.storage.DimensionDataStorage;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.neoforged.neoforge.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public class BackpackStorage extends SavedData {
	private static final SavedDataType<BackpackStorage> TYPE = new SavedDataType<>(SophisticatedBackpacks.MOD_ID, BackpackStorage::new,
			RecordCodecBuilder.create(
					builder -> builder.group(
							Codec.unboundedMap(
									Codec.STRING.xmap(UUID::fromString, UUID::toString),
									CompoundTag.CODEC
							).fieldOf("backpackContents").forGetter(storage -> storage.backpackContents),
							Codec.unboundedMap(
									Codec.STRING.xmap(UUID::fromString, UUID::toString), AccessLogRecord.CODEC
							).fieldOf("accessLogRecords").forGetter(storage -> storage.accessLogRecords)
					).apply(builder, BackpackStorage::new)
			));

	private final Map<UUID, CompoundTag> backpackContents = new HashMap<>();
	private static final BackpackStorage clientStorageCopy = new BackpackStorage();
	private final Map<UUID, AccessLogRecord> accessLogRecords = new HashMap<>();

	private BackpackStorage(Map<UUID, CompoundTag> backpackContents, Map<UUID, AccessLogRecord> accessLogRecords) {
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
				DimensionDataStorage storage = overworld.getDataStorage();
				return storage.computeIfAbsent(TYPE);
			}
		}
		return clientStorageCopy;
	}

	private static boolean isPlayerBackpackOrNotEmpty(BackpackStorage storage, UUID backpackUuid, CompoundTag contentsNbt) {
		if (storage.accessLogRecords.containsKey(backpackUuid)) {
			return true;
		}
		if (contentsNbt.contains("inventory")) {
			return contentsNbt.getCompound("inventory").map(inventoryNbt -> {
				if (inventoryNbt.contains("Items")) {
					return inventoryNbt.getList("Items").isPresent();
				}
				return false;
			}).orElse(false);
		}
		return false;
	}

	public CompoundTag getOrCreateBackpackContents(UUID backpackUuid) {
		return backpackContents.computeIfAbsent(backpackUuid, uuid -> {
			setDirty();
			return new CompoundTag();
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

	public void setBackpackContents(UUID backpackUuid, CompoundTag contents) {
		if (!backpackContents.containsKey(backpackUuid)) {
			backpackContents.put(backpackUuid, contents);
			updatedBackpackSettingsFlags.add(backpackUuid);
		} else {
			CompoundTag currentContents = backpackContents.get(backpackUuid);
			for (String key : contents.keySet()) {
				//noinspection ConstantConditions - the key is one of the tag keys so there's no reason it wouldn't exist here
				currentContents.put(key, contents.get(key));

				if (key.equals(BackpackSettingsHandler.SETTINGS_TAG)) {
					updatedBackpackSettingsFlags.add(backpackUuid);
				}
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
			if (!accessLogRecords.containsKey(entry.getKey()) && (!onlyWithEmptyInventory || !isPlayerBackpackOrNotEmpty(this, entry.getKey(), entry.getValue()))) {
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
