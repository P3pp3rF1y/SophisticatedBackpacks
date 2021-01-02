package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IUpgradeWrapperAccessor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InceptionWrapperAccessor implements IUpgradeWrapperAccessor {
	private final Map<Class<?>, List<?>> interfaceWrappers = new HashMap<>();
	private final IBackpackWrapper backpackWrapper;
	private final SubBackpacksHandler subBackpacksHandler;

	public InceptionWrapperAccessor(IBackpackWrapper backpackWrapper, SubBackpacksHandler subBackpacksHandler) {
		this.backpackWrapper = backpackWrapper;
		this.subBackpacksHandler = subBackpacksHandler;
		subBackpacksHandler.addRefreshListener(this::clearCache);
	}

	@Override
	public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
		//noinspection unchecked
		return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, this::collectListOfWrappersThatImplement);
	}

	private <T> List<T> collectListOfWrappersThatImplement(Class<T> upgradeClass) {
		List<T> ret = new ArrayList<>(backpackWrapper.getUpgradeHandler().getListOfWrappersThatImplement(upgradeClass));
		subBackpacksHandler.getSubBackpacks().forEach(sbp -> ret.addAll(sbp.getUpgradeHandler().getWrappersThatImplement(upgradeClass)));
		return ret;
	}

	@Override
	public void clearCache() {
		interfaceWrappers.clear();
	}
}
