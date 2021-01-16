package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import java.util.List;

public interface IUpgradeWrapperAccessor {
	<T> List<T> getWrappersThatImplement(Class<T> upgradeClass);

	<T> List<T> getWrappersThatImplementFromMainBackpack(Class<T> upgradeClass);

	default void onBeforeDeconstruct() {
		//noop
	}
}
