package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.INBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import net.minecraft.world.storage.WorldSavedData;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fml.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

public class BackpackStorage extends WorldSavedData {
	private static final String SAVED_DATA_NAME = SophisticatedBackpacks.MOD_ID;

	private final Map<UUID, CompoundNBT> backpackContents = new HashMap<>();
	private final Multimap<UUID, UUID> originalUuidBackpacks = HashMultimap.create();
	private static final BackpackStorage clientStorageCopy = new BackpackStorage();

	public BackpackStorage() {
		super(SAVED_DATA_NAME);
	}

	public static BackpackStorage get() {
		MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
		if (server != null) {
			ServerWorld overworld = server.getWorld(World.OVERWORLD);
			//noinspection ConstantConditions - by this time overworld is loaded
			DimensionSavedDataManager storage = overworld.getSavedData();
			return storage.getOrCreate(BackpackStorage::new, SAVED_DATA_NAME);
		}

		return clientStorageCopy;
	}

	@Override
	public void read(CompoundNBT nbt) {
		for (INBT n : nbt.getList("backpackContents", Constants.NBT.TAG_COMPOUND)) {
			CompoundNBT uuidContentsPair = (CompoundNBT) n;
			UUID uuid = NBTUtil.readUniqueId(Objects.requireNonNull(uuidContentsPair.get("uuid")));
			CompoundNBT contents = uuidContentsPair.getCompound("contents");
			backpackContents.put(uuid, contents);
		}
		for (INBT n : nbt.getList("originalUuidBackpacks", Constants.NBT.TAG_COMPOUND)) {
			CompoundNBT originalUuidBackpackPair = (CompoundNBT) n;
			UUID originalUuid = NBTUtil.readUniqueId(Objects.requireNonNull(originalUuidBackpackPair.get("originalUuid")));
			UUID uuid = NBTUtil.readUniqueId(Objects.requireNonNull(originalUuidBackpackPair.get("uuid")));
			originalUuidBackpacks.put(originalUuid, uuid);
		}
	}

	@Override
	public CompoundNBT write(CompoundNBT compound) {
		CompoundNBT ret = new CompoundNBT();
		ListNBT backpackContentsNbt = new ListNBT();
		for (Map.Entry<UUID, CompoundNBT> entry : backpackContents.entrySet()) {
			CompoundNBT uuidContentsPair = new CompoundNBT();
			uuidContentsPair.put("uuid", NBTUtil.func_240626_a_(entry.getKey()));
			uuidContentsPair.put("contents", entry.getValue());
			backpackContentsNbt.add(uuidContentsPair);
		}
		ret.put("backpackContents", backpackContentsNbt);

		for (Map.Entry<UUID, UUID> entry : originalUuidBackpacks.entries()) {
			CompoundNBT originalUuidBackpackPair = new CompoundNBT();
			originalUuidBackpackPair.put("originalUuid", NBTUtil.func_240626_a_(entry.getKey()));
			originalUuidBackpackPair.put("uuid", NBTUtil.func_240626_a_(entry.getValue()));
			backpackContentsNbt.add(originalUuidBackpackPair);
		}
		ret.put("originalUuidBackpacks", backpackContentsNbt);
		return ret;
	}

	public CompoundNBT getOrCreateBackpackContents(UUID backpackUuid, @Nullable UUID originalUuid) {
		if (originalUuid != null && !originalUuidBackpacks.containsValue(backpackUuid)) {
			for (UUID existingBackpackUuid : originalUuidBackpacks.removeAll(originalUuid)) {
				backpackContents.remove(existingBackpackUuid);
			}
			originalUuidBackpacks.put(originalUuid, backpackUuid);
		}

		return getOrCreateBackpackContents(backpackUuid);
	}

	public CompoundNBT getOrCreateBackpackContents(UUID backpackUuid) {
		return backpackContents.computeIfAbsent(backpackUuid, uuid -> new CompoundNBT());
	}

	public void removeOriginalBackpack(@Nullable UUID originalUuid) {
		if (originalUuid == null) {
			return;
		}

		backpackContents.remove(originalUuid);
	}
}
