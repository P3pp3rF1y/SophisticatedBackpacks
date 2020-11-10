package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

public class UpgradeContainerRegistry {
	private UpgradeContainerRegistry() {}

	private static final Map<ResourceLocation, UpgradeContainerType<? extends UpgradeContainerBase>> UPGRADE_CONTAINERS = new HashMap<>();

	public static void register(ResourceLocation upgradeName, UpgradeContainerType<? extends UpgradeContainerBase> containerFactory) {
		UPGRADE_CONTAINERS.put(upgradeName, containerFactory);
	}

	public static Optional<UpgradeContainerBase> instantiateContainer(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		ResourceLocation upgradeName = upgrade.getItem().getRegistryName();
		if (UPGRADE_CONTAINERS.containsKey(upgradeName)) {
			return Optional.of(UPGRADE_CONTAINERS.get(upgradeName).create(upgrade, upgradeSaveHandler));
		}
		return Optional.empty();
	}
}
