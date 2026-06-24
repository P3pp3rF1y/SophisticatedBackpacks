package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.resources.ResourceLocation;
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

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class BackpackTemplateStorage extends SavedData {
	private static final SavedDataType<BackpackTemplateStorage> TYPE = new SavedDataType<>(SophisticatedBackpacks.MOD_ID + "_backpack_templates",
			BackpackTemplateStorage::new, RecordCodecBuilder.create(builder -> builder.group(Codec.unboundedMap(ResourceLocation.CODEC, BackpackTemplate.CODEC)
					.fieldOf("backpackTemplates").forGetter(storage -> storage.backpackTemplates)).apply(builder, BackpackTemplateStorage::new)));

	private static final BackpackTemplateStorage clientStorageCopy = new BackpackTemplateStorage();
	private final Map<ResourceLocation, BackpackTemplate> backpackTemplates = new HashMap<>();

	private BackpackTemplateStorage(Map<ResourceLocation, BackpackTemplate> backpackTemplates) {
		this.backpackTemplates.putAll(backpackTemplates);
	}

	private BackpackTemplateStorage() {
	}

	public static BackpackTemplateStorage get() {
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

	public Optional<BackpackTemplate> getBackpackTemplate(ResourceLocation templateName) {
		return Optional.ofNullable(backpackTemplates.get(templateName));
	}

	public void setBackpackTemplate(ResourceLocation templateName, BackpackTemplate backpackTemplate) {
		backpackTemplates.put(templateName, backpackTemplate);
		setDirty();
	}

	public void removeBackpackTemplate(ResourceLocation templateName) {
		backpackTemplates.remove(templateName);
		setDirty();
	}

	public Map<ResourceLocation, BackpackTemplate> getBackpackTemplates() {
		return backpackTemplates;
	}

	public static void onClientWorldLoad(LevelEvent.Load evt) {
		if (evt.getLevel().isClientSide()) {
			clientStorageCopy.backpackTemplates.clear();
		}
	}
}
