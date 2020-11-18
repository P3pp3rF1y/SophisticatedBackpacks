package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class RegistryHelper {
	private RegistryHelper() {}

	public static String getModRegistryName(String regName) {
		return SophisticatedBackpacks.MOD_ID + ":" + regName;
	}
}