package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
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
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.util.CodecHelper;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

//TODO after 1.22 remove support for legacy UUID deserialization via strings
public class BackpackStorage extends SavedData {
	private static final Codec<BackpackStorage> CODEC = Codec.withAlternative(
			RecordCodecBuilder.create(builder -> builder.group(
					Codec.unboundedMap(CodecHelper.STRING_ENCODED_UUID, ContainerContents.CODEC).fieldOf("backpackContents")
							.forGetter(storage -> storage.backpackContents),
					Codec.unboundedMap(CodecHelper.STRING_ENCODED_UUID, AccessLogRecord.CODEC).fieldOf("accessLogRecords")
							.forGetter(storage -> storage.accessLogRecords),
					Codec.unboundedMap(CodecHelper.STRING_ENCODED_UUID, CompoundTag.CODEC).optionalFieldOf("additionalBackpackContents", Map.of())
							.forGetter(storage -> storage.additionalBackpackContents))
					.apply(builder, BackpackStorage::new)),
			CompoundTag.CODEC, BackpackStorage::legacyDeserialize);
	private static final SavedDataType<BackpackStorage> TYPE = new SavedDataType<>(SophisticatedBackpacks.MOD_ID, BackpackStorage::new, CODEC);

	private final Map<UUID, ContainerContents> backpackContents = new HashMap<>();
	private static final BackpackStorage clientStorageCopy = new BackpackStorage();
	private final Map<UUID, AccessLogRecord> accessLogRecords = new HashMap<>();
	private final Map<UUID, CompoundTag> additionalBackpackContents = new HashMap<>();

	private BackpackStorage(Map<UUID, ContainerContents> backpackContents, Map<UUID, AccessLogRecord> accessLogRecords,
			Map<UUID, CompoundTag> additionalBackpackContents) {
		this.accessLogRecords.putAll(accessLogRecords);
		this.additionalBackpackContents.putAll(additionalBackpackContents);
		backpackContents.forEach((uuid, contents) -> {
			if (isPlayerBackpackOrNotEmpty(this, uuid, contents)) {
				this.backpackContents.put(uuid, contents);
			}
		});
	}

	private BackpackStorage() {
	}

	public static BackpackStorage get() {
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
			if (server != null) {
				ServerLevel overworld = server.getLevel(Level.OVERWORLD);
				// noinspection ConstantConditions - by this time overworld is loaded
				DimensionDataStorage storage = overworld.getDataStorage();
				return storage.computeIfAbsent(TYPE);
			}
		}
		return clientStorageCopy;
	}

	static BackpackStorage legacyDeserialize(CompoundTag nbt) {
		Map<UUID, AccessLogRecord> accessLogRecords = new HashMap<>();
		readLegacyAccessLogs(nbt, accessLogRecords);

		Map<UUID, ContainerContents> backpackContents = new HashMap<>();
		readLegacyBackpackContents(nbt, backpackContents);
		return new BackpackStorage(backpackContents, accessLogRecords, Map.of());
	}

	private static void readLegacyAccessLogs(CompoundTag nbt, Map<UUID, AccessLogRecord> accessLogRecords) {
		nbt.getListOrEmpty("accessLogRecords").compoundStream()
				.forEach(accessLogTag -> AccessLogRecord.CODEC.parse(NbtOps.INSTANCE, accessLogTag)
						.resultOrPartial(error -> SophisticatedBackpacks.LOGGER.error("Failed to parse legacy backpack access log: {}", error))
						.ifPresent(accessLogRecord -> accessLogRecords.put(accessLogRecord.backpackUuid(), accessLogRecord)));
	}

	private static void readLegacyBackpackContents(CompoundTag nbt, Map<UUID, ContainerContents> backpackContents) {
		nbt.getListOrEmpty("backpackContents").compoundStream().forEach(uuidContentsPair -> {
			Tag uuidTag = uuidContentsPair.get("uuid");
			if (uuidTag == null) {
				return;
			}

			uuidContentsPair.getCompound("contents").ifPresent(contentsTag -> UUIDUtil.CODEC.parse(NbtOps.INSTANCE, uuidTag)
					.resultOrPartial(error -> SophisticatedBackpacks.LOGGER.error("Failed to parse legacy backpack uuid: {}", error))
					.ifPresent(uuid -> ContainerContents.CODEC.parse(NbtOps.INSTANCE, contentsTag)
							.resultOrPartial(error -> SophisticatedBackpacks.LOGGER.error("Failed to parse legacy backpack contents for {}: {}", uuid, error))
							.ifPresent(contents -> backpackContents.put(uuid, contents))));
		});
	}

	private static boolean isPlayerBackpackOrNotEmpty(BackpackStorage storage, UUID backpackUuid, ContainerContents contents) {
		if (storage.accessLogRecords.containsKey(backpackUuid)) {
			return true;
		}
		return !contents.inventory().stacks().isEmpty() || !storage.additionalBackpackContents.getOrDefault(backpackUuid, new CompoundTag()).isEmpty();
	}

	public ContainerContents getOrCreateBackpackContents(UUID backpackUuid) {
		return backpackContents.computeIfAbsent(backpackUuid, uuid -> {
			setDirty();
			return new ContainerContents();
		});
	}

	public CompoundTag getOrCreateAdditionalBackpackContents(UUID backpackUuid) {
		return additionalBackpackContents.computeIfAbsent(backpackUuid, uuid -> {
			setDirty();
			return new CompoundTag();
		});
	}

	public void setAdditionalBackpackContents(UUID backpackUuid, CompoundTag contents) {
		CompoundTag currentContents = getOrCreateAdditionalBackpackContents(backpackUuid);
		for (String key : contents.keySet()) {
			currentContents.put(key, contents.get(key));
		}
		setDirty();
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
			if (!accessLogRecords.containsKey(entry.getKey())
					&& (!onlyWithEmptyInventory || !isPlayerBackpackOrNotEmpty(this, entry.getKey(), entry.getValue()))) {
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
