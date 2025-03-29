package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;

import javax.annotation.Nullable;
import java.util.Set;

public class BackpackTemplates {

	private BackpackTemplates() {
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

	public static Set<String> getTemplateNames() {
		return BackpackStorage.get().getBackpackTemplates().keySet();
	}
}
