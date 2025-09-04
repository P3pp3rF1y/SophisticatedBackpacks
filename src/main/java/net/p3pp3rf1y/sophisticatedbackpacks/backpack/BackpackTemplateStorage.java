package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.storage.DimensionDataStorage;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.neoforged.neoforge.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class BackpackTemplateStorage extends SavedData {
	private static final String SAVED_DATA_NAME = SophisticatedBackpacks.MOD_ID + "_backpack_templates";

	private static final BackpackTemplateStorage clientStorageCopy = new BackpackTemplateStorage();
	private final Map<ResourceLocation, CompoundTag> backpackTemplates = new HashMap<>();

	private BackpackTemplateStorage() {
	}

	public static BackpackTemplateStorage get() {
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
			if (server != null) {
				ServerLevel overworld = server.getLevel(Level.OVERWORLD);
				//noinspection ConstantConditions - by this time overworld is loaded
				DimensionDataStorage storage = overworld.getDataStorage();
				return storage.computeIfAbsent(new Factory<>(BackpackTemplateStorage::new, BackpackTemplateStorage::load), SAVED_DATA_NAME);
			}
		}
		return clientStorageCopy;
	}

	public static BackpackTemplateStorage load(CompoundTag nbt, HolderLookup.Provider registries) {
		BackpackTemplateStorage storage = new BackpackTemplateStorage();
		readBackpackTemplates(nbt, storage);
		return storage;
	}

	private static void readBackpackTemplates(CompoundTag nbt, BackpackTemplateStorage storage) {
		if (!nbt.contains("backpackTemplates")) {
			return;
		}

		ListTag list = nbt.getList("backpackTemplates", Tag.TAG_COMPOUND);
		for (int i = 0; i < list.size(); i++) {
			CompoundTag tag = list.getCompound(i);
			ResourceLocation templateName = ResourceLocation.parse(tag.getString("templateName"));
			CompoundTag contents = tag.getCompound("contents");
			storage.backpackTemplates.put(templateName, contents);
		}
	}

	@Override
	public CompoundTag save(CompoundTag compound, HolderLookup.Provider registries) {
		CompoundTag ret = new CompoundTag();
		writeBackpackTemplates(ret);
		return ret;
	}

	private void writeBackpackTemplates(CompoundTag ret) {
		if (backpackTemplates.isEmpty()) {
			return;
		}

		ListTag backpackTemplatesNbt = new ListTag();
		for (Map.Entry<ResourceLocation, CompoundTag> entry : backpackTemplates.entrySet()) {
			CompoundTag ret1 = new CompoundTag();
			ret1.putString("templateName", entry.getKey().toString());
			ret1.put("contents", entry.getValue());
			backpackTemplatesNbt.add(ret1);
		}
		ret.put("backpackTemplates", backpackTemplatesNbt);
	}

	public Optional<CompoundTag> getBackpackTemplate(ResourceLocation templateName) {
		return Optional.ofNullable(backpackTemplates.get(templateName));
	}

	public void setBackpackTemplate(ResourceLocation templateName, CompoundTag contents) {
		backpackTemplates.put(templateName, contents);
		setDirty();
	}

	public void removeBackpackTemplate(ResourceLocation templateName) {
		backpackTemplates.remove(templateName);
		setDirty();
	}

	public Map<ResourceLocation, CompoundTag> getBackpackTemplates() {
		return backpackTemplates;
	}

	public static void onClientWorldLoad(LevelEvent.Load evt) {
		if (evt.getLevel().isClientSide()) {
			clientStorageCopy.backpackTemplates.clear();
		}
	}
}
