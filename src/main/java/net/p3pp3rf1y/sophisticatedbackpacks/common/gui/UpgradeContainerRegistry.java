package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class UpgradeContainerRegistry {
	private UpgradeContainerRegistry() {}

	private static final Map<ResourceLocation, UpgradeContainerType<? extends IUpgradeWrapper, ? extends UpgradeContainerBase<?>>> UPGRADE_CONTAINERS = new HashMap<>();

	public static void register(ResourceLocation upgradeName, UpgradeContainerType<? extends IUpgradeWrapper, ? extends UpgradeContainerBase<?>> containerFactory) {
		UPGRADE_CONTAINERS.put(upgradeName, containerFactory);
	}

	public static <W extends IUpgradeWrapper> Optional<UpgradeContainerBase<W>> instantiateContainer(int containerId, W wrapper, boolean isClientSide) {
		ResourceLocation upgradeName = wrapper.getUpgradeStack().getItem().getRegistryName();
		if (!(wrapper.getUpgradeStack().getItem() instanceof IBackpackUpgradeItem<?>) || !UPGRADE_CONTAINERS.containsKey(upgradeName)) {
			return Optional.empty();
		}
		//noinspection unchecked,ConstantConditions
		return Optional.of((UpgradeContainerBase<W>) getContainerType(upgradeName).create(containerId, wrapper, isClientSide));
	}

	private static <W extends IUpgradeWrapper, T extends UpgradeContainerBase<W>> UpgradeContainerType<W, T> getContainerType(ResourceLocation upgradeName) {
		//noinspection unchecked
		return (UpgradeContainerType<W, T>) UPGRADE_CONTAINERS.get(upgradeName);
	}
}
