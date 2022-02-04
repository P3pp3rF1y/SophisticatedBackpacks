package net.p3pp3rf1y.sophisticatedcore.upgrades;

import java.util.List;

public interface IUpgradeWrapperAccessor {
	<T> List<T> getWrappersThatImplement(Class<T> upgradeClass);

	<T> List<T> getWrappersThatImplementFromMainStorage(Class<T> upgradeClass);

	void clearCache();

	default void onBeforeDeconstruct() {
		//noop
	}
}
