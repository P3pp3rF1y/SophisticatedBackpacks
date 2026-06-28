package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BackpackStorageTest {
	@Test
	void legacyBackpackStorageListsDeserializeToCurrentStorage() {
		UUID backpackUuid = new UUID(1, 2);
		CompoundTag legacyStorage = new CompoundTag();
		legacyStorage.put("accessLogRecords", legacyAccessLogs(backpackUuid));
		legacyStorage.put("backpackContents", legacyBackpackContents(backpackUuid));

		BackpackStorage storage = BackpackStorage.legacyDeserialize(legacyStorage);

		assertEquals(1, storage.getAccessLogs().size());
		assertEquals("Player", storage.getAccessLogs().get(backpackUuid).playerName());
		assertEquals(2, storage.getOrCreateBackpackContents(backpackUuid).inventory().stacks().size());
	}

	private static ListTag legacyAccessLogs(UUID backpackUuid) {
		ListTag accessLogs = new ListTag();
		CompoundTag accessLog = new CompoundTag();
		accessLog.putString("backpackItemRegistryName", "sophisticatedbackpacks:backpack");
		accessLog.put("backpackUuid", UUIDUtil.CODEC.encodeStart(NbtOps.INSTANCE, backpackUuid).getOrThrow());
		accessLog.putString("playerName", "Player");
		accessLog.putString("backpackName", "Backpack");
		accessLog.putInt("clothColor", -1);
		accessLog.putInt("trimColor", -1);
		accessLog.putLong("accessTime", 123L);
		accessLog.putInt("columnsTaken", 9);
		accessLogs.add(accessLog);
		return accessLogs;
	}

	private static ListTag legacyBackpackContents(UUID backpackUuid) {
		ListTag backpackContents = new ListTag();
		CompoundTag uuidContentsPair = new CompoundTag();
		uuidContentsPair.put("uuid", UUIDUtil.CODEC.encodeStart(NbtOps.INSTANCE, backpackUuid).getOrThrow());
		CompoundTag contents = new CompoundTag();
		CompoundTag inventory = new CompoundTag();
		inventory.putInt("Size", 2);
		inventory.put("Items", new ListTag());
		contents.put("inventory", inventory);
		uuidContentsPair.put("contents", contents);
		backpackContents.add(uuidContentsPair);
		return backpackContents;
	}
}
