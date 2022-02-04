package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapperAccessor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InceptionWrapperAccessor implements IUpgradeWrapperAccessor {
	private final Map<Class<?>, List<?>> interfaceWrappers = new HashMap<>();
	private final Map<Class<?>, List<?>> mainBackpackInterfaceWrappers = new HashMap<>();
	private final IStorageWrapper backpackWrapper;
	private final SubBackpacksHandler subBackpacksHandler;

	public InceptionWrapperAccessor(IStorageWrapper backpackWrapper, SubBackpacksHandler subBackpacksHandler) {
		this.backpackWrapper = backpackWrapper;
		this.subBackpacksHandler = subBackpacksHandler;
		addRefreshCallbacks(subBackpacksHandler.getSubBackpacks());
		subBackpacksHandler.addBeforeRefreshListener(this::removeCallBacks);
		subBackpacksHandler.addRefreshListener(this::clearCacheAndAddCallBacks);
	}

	private void clearCacheAndAddCallBacks(Collection<IStorageWrapper> subbackpacks) {
		clearCache();
		addRefreshCallbacks(subbackpacks);
	}

	private void addRefreshCallbacks(Collection<IStorageWrapper> subbackpacks) {
		subbackpacks.forEach(sb -> sb.getUpgradeHandler().setRefreshCallBack(this::clearCache));
	}

	@Override
	public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
		//noinspection unchecked
		return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, this::collectListOfWrappersThatImplement);
	}

	@Override
	public <T> List<T> getWrappersThatImplementFromMainStorage(Class<T> upgradeClass) {
		//noinspection unchecked
		return (List<T>) mainBackpackInterfaceWrappers.computeIfAbsent(upgradeClass, backpackWrapper.getUpgradeHandler()::getListOfWrappersThatImplement);
	}

	@Override
	public void onBeforeDeconstruct() {
		removeCallBacks(subBackpacksHandler.getSubBackpacks());
	}

	private <T> List<T> collectListOfWrappersThatImplement(Class<T> upgradeClass) {
		List<T> ret = new ArrayList<>(backpackWrapper.getUpgradeHandler().getListOfWrappersThatImplement(upgradeClass));
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> ret.addAll(sbp.getUpgradeHandler().getWrappersThatImplement(upgradeClass)));
		return ret;
	}

	private void removeCallBacks(Collection<IStorageWrapper> subBackpacksHandler) {
		subBackpacksHandler.forEach(sb -> sb.getUpgradeHandler().removeRefreshCallback());
	}

	@Override
	public void clearCache() {
		interfaceWrappers.clear();
	}
}
