package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import java.util.List;

public interface IUpgradeWrapperAccessor {
	<T> List<T> getWrappersThatImplement(Class<T> upgradeClass);

	void clearCache();
}
