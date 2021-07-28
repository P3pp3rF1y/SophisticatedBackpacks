package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.Item;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.ForgeRegistryEntry;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import org.apache.commons.lang3.Validate;

import java.util.Optional;

public class RegistryHelper {
	private RegistryHelper() {}

	public static ResourceLocation getRL(String regName) {
		return new ResourceLocation(getModRegistryName(regName));
	}

	public static String getModRegistryName(String regName) {
		return SophisticatedBackpacks.MOD_ID + ":" + regName;
	}

	public static ResourceLocation getItemKey(Item item) {
		ResourceLocation itemKey = ForgeRegistries.ITEMS.getKey(item);
		Validate.notNull(itemKey, "itemKey");
		return itemKey;
	}

	public static Optional<ResourceLocation> getRegistryName(ForgeRegistryEntry<?> registryEntry) {
		return Optional.ofNullable(registryEntry.getRegistryName());
	}
}