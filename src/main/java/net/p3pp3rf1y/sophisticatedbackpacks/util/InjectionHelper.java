package net.p3pp3rf1y.sophisticatedbackpacks.util;

import javax.annotation.Nullable;

public class InjectionHelper {
	private InjectionHelper() {}

	@Nullable
	public static <T> T nullValue() {
		return null;
	}
}