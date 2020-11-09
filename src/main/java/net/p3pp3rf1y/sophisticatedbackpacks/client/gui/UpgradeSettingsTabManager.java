package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public class UpgradeSettingsTabManager {
	private UpgradeSettingsTabManager() {}

	private static final Map<UpgradeContainerType<?>, IUpgradeSettingsFactory<?, ?>> UPGRADE_TABS = new HashMap<>();

	public static <C extends UpgradeContainerBase, S extends UpgradeSettingsTab<C>> void register(UpgradeContainerType<C> containerType, IUpgradeSettingsFactory<C, S> upgradeSettingsFactory) {
		UPGRADE_TABS.put(containerType, upgradeSettingsFactory);
	}

	public static <C extends UpgradeContainerBase> UpgradeSettingsTab<C> getTab(C container, int x, int y, BackpackScreen screen,
			Consumer<UpgradeSettingsTab<C>> onOpen, Consumer<UpgradeSettingsTab<C>> onClose) {
		return getFactory(container).create(container, x, y, screen, onOpen, onClose);
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
		S create(C container, int x, int y, BackpackScreen screen, Consumer<S> onOpen, Consumer<S> onClose);
	}
}
