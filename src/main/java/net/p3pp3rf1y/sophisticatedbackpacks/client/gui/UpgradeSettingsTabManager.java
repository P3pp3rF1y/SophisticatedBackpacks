package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

import java.util.HashMap;
import java.util.Map;

@OnlyIn(Dist.CLIENT)
public class UpgradeSettingsTabManager {
	private UpgradeSettingsTabManager() {}

	private static final Map<UpgradeContainerType<?>, IUpgradeSettingsFactory<?, ?>> UPGRADE_TABS = new HashMap<>();

	public static <C extends UpgradeContainerBase, S extends UpgradeSettingsTab<C>> void register(UpgradeContainerType<C> containerType, IUpgradeSettingsFactory<C, S> upgradeSettingsFactory) {
		UPGRADE_TABS.put(containerType, upgradeSettingsFactory);
	}

	public static <C extends UpgradeContainerBase> UpgradeSettingsTab<C> getTab(C container, Position position, BackpackScreen screen) {
		return getFactory(container).create(container, position, screen);
	}

	@SuppressWarnings("unchecked")
	private static <C extends UpgradeContainerBase, S extends UpgradeSettingsTab<C>> IUpgradeSettingsFactory<C, S> getFactory(C container) {
		return (IUpgradeSettingsFactory<C, S>) getFactory(container.getType());
	}

	@SuppressWarnings("unchecked")
	private static <C extends UpgradeContainerBase, S extends UpgradeSettingsTab<C>> IUpgradeSettingsFactory<C, S> getFactory(UpgradeContainerType<C> containerType) {
		return (IUpgradeSettingsFactory<C, S>) UPGRADE_TABS.get(containerType);
	}

	public interface IUpgradeSettingsFactory<C extends UpgradeContainerBase, S extends UpgradeSettingsTab<C>> {
		S create(C container, Position position, BackpackScreen screen);
	}
}
