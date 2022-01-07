package net.p3pp3rf1y.sophisticatedbackpacks.api;

import java.util.List;

public interface IUpgradeWrapperAccessor {
	<T> List<T> getWrappersThatImplement(Class<T> upgradeClass);

	<T> List<T> getWrappersThatImplementFromMainBackpack(Class<T> upgradeClass);

	void clearCache();

	default void onBeforeDeconstruct() {
		//noop
	}
}
