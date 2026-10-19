package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapperAccessor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

public class InceptionWrapperAccessor implements IUpgradeWrapperAccessor {
	private final Map<Class<?>, List<?>> interfaceWrappers = new ConcurrentHashMap<>();
	private final Map<Class<?>, List<?>> mainBackpackInterfaceWrappers = new ConcurrentHashMap<>();
	private final IStorageWrapper backpackWrapper;
	private final SubBackpacksHandler subBackpacksHandler;
	private final Runnable refreshCallback = this::onUpgradeCachesInvalidated;
	private final Consumer<Collection<IStorageWrapper>> beforeRefreshListener = this::removeCallBacks;
	private final Consumer<Collection<IStorageWrapper>> refreshListener = this::clearCacheAndAddCallBacks;

	public InceptionWrapperAccessor(IStorageWrapper backpackWrapper, SubBackpacksHandler subBackpacksHandler) {
		this.backpackWrapper = backpackWrapper;
		this.subBackpacksHandler = subBackpacksHandler;
		addRefreshCallbacks(subBackpacksHandler.getSubBackpacks());
		subBackpacksHandler.addBeforeRefreshListener(beforeRefreshListener);
		subBackpacksHandler.addRefreshListener(refreshListener);
	}

	private void clearCacheAndAddCallBacks(Collection<IStorageWrapper> subbackpacks) {
		clearCache();
		addRefreshCallbacks(subbackpacks);
	}

	private void addRefreshCallbacks(Collection<IStorageWrapper> subbackpacks) {
		subbackpacks.forEach(sb -> sb.getUpgradeHandler().addRefreshCallback(refreshCallback));
	}

	private void onUpgradeCachesInvalidated() {
		clearCache();
		addRefreshCallbacks(subBackpacksHandler.getSubBackpacks());
		subBackpacksHandler.onUpgradeCachesInvalidated();
		backpackWrapper.getUpgradeHandler().notifyRefreshCallbacks();
	}

	@Override
	public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
		// noinspection unchecked
		return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, this::collectListOfWrappersThatImplement);
	}

	@Override
	public <T> List<T> getWrappersThatImplementFromMainStorage(Class<T> upgradeClass) {
		// noinspection unchecked
		return (List<T>) mainBackpackInterfaceWrappers.computeIfAbsent(upgradeClass, backpackWrapper.getUpgradeHandler()::getListOfWrappersThatImplement);
	}

	@Override
	public void onBeforeDeconstruct() {
		removeCallBacks(subBackpacksHandler.getSubBackpacks());
		subBackpacksHandler.removeBeforeRefreshListener(beforeRefreshListener);
		subBackpacksHandler.removeRefreshListener(refreshListener);
	}

	private <T> List<T> collectListOfWrappersThatImplement(Class<T> upgradeClass) {
		List<T> ret = new ArrayList<>(backpackWrapper.getUpgradeHandler().getListOfWrappersThatImplement(upgradeClass));
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> ret.addAll(sbp.getUpgradeHandler().getWrappersThatImplement(upgradeClass)));
		return ret;
	}

	private void removeCallBacks(Collection<IStorageWrapper> subBackpacksHandler) {
		subBackpacksHandler.forEach(sb -> sb.getUpgradeHandler().removeRefreshCallback(refreshCallback));
	}

	@Override
	public void clearCache() {
		interfaceWrappers.clear();
		mainBackpackInterfaceWrappers.clear();
	}
}
