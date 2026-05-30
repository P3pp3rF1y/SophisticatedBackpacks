package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.nbt.Tag;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.storage.DimensionDataStorage;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.neoforged.neoforge.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LegacyBackpackDataMigration;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public class BackpackStorage extends SavedData {
	private static final String SAVED_DATA_NAME = SophisticatedBackpacks.MOD_ID;

	private final Map<UUID, CompoundTag> backpackContents = new HashMap<>();
	private static final BackpackStorage clientStorageCopy = new BackpackStorage();
	private final Map<UUID, AccessLogRecord> accessLogRecords = new HashMap<>();

	private BackpackStorage() {
	}

	public static BackpackStorage get() {
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
			if (server != null) {
				ServerLevel overworld = server.getLevel(Level.OVERWORLD);
				//noinspection ConstantConditions - by this time overworld is loaded
				DimensionDataStorage storage = overworld.getDataStorage();
				return storage.computeIfAbsent(new Factory<>(BackpackStorage::new, BackpackStorage::load), SAVED_DATA_NAME);
			}
		}
		return clientStorageCopy;
	}

	public static BackpackStorage load(CompoundTag nbt, HolderLookup.Provider registries) {
		BackpackStorage storage = new BackpackStorage();
		readAccessLogs(nbt, storage);
		readBackpackContents(nbt, storage);
		return storage;
	}

	private static void readAccessLogs(CompoundTag nbt, BackpackStorage storage) {
		for (Tag n : nbt.getList("accessLogRecords", Tag.TAG_COMPOUND)) {
			AccessLogRecord alr = AccessLogRecord.deserializeFromNBT((CompoundTag) n);
			storage.accessLogRecords.put(alr.getBackpackUuid(), alr);
		}
	}

	private static void readBackpackContents(CompoundTag nbt, BackpackStorage storage) {
		for (Tag n : nbt.getList("backpackContents", Tag.TAG_COMPOUND)) {
			CompoundTag uuidContentsPair = (CompoundTag) n;
			UUID uuid = NbtUtils.loadUUID(Objects.requireNonNull(uuidContentsPair.get("uuid")));
			CompoundTag contents = uuidContentsPair.getCompound("contents");
			LegacyBackpackDataMigration.normalizeLegacyBackpackContents(contents);
			if (isPlayerBackpackOrNotEmpty(storage, uuid, contents)) {
				storage.backpackContents.put(uuid, contents);
			}
		}
	}

	private static boolean isPlayerBackpackOrNotEmpty(BackpackStorage storage, UUID backpackUuid, CompoundTag contentsNbt) {
		if (storage.accessLogRecords.containsKey(backpackUuid)) {
			return true;
		}
		return hasItems(contentsNbt, InventoryHandler.INVENTORY_TAG) || hasItems(contentsNbt, UpgradeHandler.UPGRADE_INVENTORY_TAG) || hasOtherBackpackData(contentsNbt);
	}

	private static boolean hasItems(CompoundTag contentsNbt, String inventoryTag) {
		if (!contentsNbt.contains(inventoryTag)) {
			return false;
		}

		CompoundTag inventoryNbt = contentsNbt.getCompound(inventoryTag);
		return inventoryNbt.contains("Items") && !inventoryNbt.getList("Items", Tag.TAG_COMPOUND).isEmpty();
	}

	private static boolean hasOtherBackpackData(CompoundTag contentsNbt) {
		return contentsNbt.getAllKeys().stream().anyMatch(key -> !key.equals(InventoryHandler.INVENTORY_TAG) && !key.equals(UpgradeHandler.UPGRADE_INVENTORY_TAG));
	}

	@Override
	public CompoundTag save(CompoundTag compound, HolderLookup.Provider registries) {
		CompoundTag ret = new CompoundTag();
		writeBackpackContents(ret);
		writeAccessLogs(ret);
		return ret;
	}

	private void writeBackpackContents(CompoundTag ret) {
		ListTag backpackContentsNbt = new ListTag();
		for (Map.Entry<UUID, CompoundTag> entry : backpackContents.entrySet()) {
			CompoundTag uuidContentsPair = new CompoundTag();
			uuidContentsPair.put("uuid", NbtUtils.createUUID(entry.getKey()));
			uuidContentsPair.put("contents", entry.getValue());
			backpackContentsNbt.add(uuidContentsPair);
		}
		ret.put("backpackContents", backpackContentsNbt);
	}

	private void writeAccessLogs(CompoundTag ret) {
		ListTag accessLogsNbt = new ListTag();
		for (AccessLogRecord alr : accessLogRecords.values()) {
			accessLogsNbt.add(alr.serializeToNBT());
		}
		ret.put("accessLogRecords", accessLogsNbt);
	}

	public CompoundTag getOrCreateBackpackContents(UUID backpackUuid) {
		return backpackContents.computeIfAbsent(backpackUuid, uuid -> {
			setDirty();
			return new CompoundTag();
		});
	}

	public void putAccessLog(AccessLogRecord alr) {
		accessLogRecords.put(alr.getBackpackUuid(), alr);
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
			for (String key : contents.getAllKeys()) {
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
