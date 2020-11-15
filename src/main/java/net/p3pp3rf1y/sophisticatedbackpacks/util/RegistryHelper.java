package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import javax.annotation.Nullable;

public class RegistryHelper {
	private RegistryHelper() {}

	@Nullable
	public static <T> T nullValue() {
		return null;
	}

	public static String getModRegistryName(String regName) {
		return SophisticatedBackpacks.MOD_ID + ":" + regName;
	}
}