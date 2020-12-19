package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class RegistryHelper {
	private RegistryHelper() {}

	public static ResourceLocation getModRegistryResourceLocation(String regName) {
		return new ResourceLocation(getModRegistryName(regName));
	}

	public static String getModRegistryName(String regName) {
		return SophisticatedBackpacks.MOD_ID + ":" + regName;
	}
}