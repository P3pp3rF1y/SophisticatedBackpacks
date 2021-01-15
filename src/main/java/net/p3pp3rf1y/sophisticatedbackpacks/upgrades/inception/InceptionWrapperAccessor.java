package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IUpgradeWrapperAccessor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InceptionWrapperAccessor implements IUpgradeWrapperAccessor {
	private final Map<Class<?>, List<?>> interfaceWrappers = new HashMap<>();
	private final Map<Class<?>, List<?>> mainBackpackInterfaceWrappers = new HashMap<>();
	private final IBackpackWrapper backpackWrapper;
	private final SubBackpacksHandler subBackpacksHandler;

	public InceptionWrapperAccessor(IBackpackWrapper backpackWrapper, SubBackpacksHandler subBackpacksHandler) {
		this.backpackWrapper = backpackWrapper;
		this.subBackpacksHandler = subBackpacksHandler;
		addRefreshCallbacks(subBackpacksHandler.getSubBackpacks());
		subBackpacksHandler.addBeforeRefreshListener(this::removeCallBacks);
		subBackpacksHandler.addRefreshListener(this::clearCacheAndAddCallBacks);
	}

	private void clearCacheAndAddCallBacks(Collection<IBackpackWrapper> subbackpacks) {
		clearCache();
		addRefreshCallbacks(subbackpacks);
	}

	private void addRefreshCallbacks(Collection<IBackpackWrapper> subbackpacks) {
		subbackpacks.forEach(sb -> sb.getUpgradeHandler().setRefreshCallBack(this::clearCache));
	}

	@Override
	public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
		//noinspection unchecked
		return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, this::collectListOfWrappersThatImplement);
	}

	@Override
	public <T> List<T> getWrappersThatImplementFromMainBackpack(Class<T> upgradeClass) {
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

	private void removeCallBacks(Collection<IBackpackWrapper> subBackpacksHandler) {
		subBackpacksHandler.forEach(sb -> sb.getUpgradeHandler().removeRefreshCallback());
	}

	private void clearCache() {
		interfaceWrappers.clear();
	}
}
