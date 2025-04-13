package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;

import javax.annotation.Nullable;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public class BackpackTemplates {

	private BackpackTemplates() {
	}

	public static void setBackpackTemplate(String templateName, IBackpackWrapper wrapper, boolean persistent) {
		Item backpackItem = wrapper.getBackpack().getItem();
		Optional<UUID> backpackUuid = wrapper.getContentsUuid();
		backpackUuid.ifPresent(uuid -> setBackpackTemplate(templateName, BuiltInRegistries.ITEM.getKey(backpackItem), BackpackStorage.get().getOrCreateBackpackContents(uuid).copy(), persistent));
	}

	public static void setBackpackTemplate(String templateName, ResourceLocation backpackItemRegistryName, CompoundTag contents, boolean persistent) {
		CompoundTag data = new CompoundTag();
		data.putString("backpackItemRegistryName", backpackItemRegistryName.toString());
		data.put("backpackContents", contents);
		data.putBoolean("persistent", persistent);
		BackpackStorage.get().setBackpackTemplate(templateName, data);
	}

	@Nullable
	public static CompoundTag getBackpackTemplate(String templateName) {
		return BackpackStorage.get().getBackpackTemplate(templateName);
	}

	public static void removeBackpackTemplate(String templateName) {
		BackpackStorage.get().removeBackpackTemplate(templateName);
	}

	public static boolean isPersistent(String templateName) {
		CompoundTag backpackTemplate = getBackpackTemplate(templateName);
		return backpackTemplate != null && backpackTemplate.getBoolean("persistent");
	}

	public static Set<String> getTemplateNames() {
		return BackpackStorage.get().getBackpackTemplates().keySet();
	}
}
