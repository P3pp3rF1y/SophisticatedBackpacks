package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class UpgradeGuiManager {
	private UpgradeGuiManager() {}

	private static final Map<UpgradeContainerType<?, ?>, IUpgradeSettingsFactory<?, ?>> UPGRADE_TABS = new HashMap<>();
	private static final Map<UpgradeContainerType<?, ?>, IUpgradeInventoryPartFactory<?, ?>> UPGRADE_INVENTORY_PARTS = new HashMap<>();

	public static <W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>, S extends UpgradeSettingsTab<C>> void registerTab(UpgradeContainerType<W, C> containerType, IUpgradeSettingsFactory<C, S> upgradeSettingsFactory) {
		UPGRADE_TABS.put(containerType, upgradeSettingsFactory);
	}

	public static <W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>, I extends UpgradeInventoryPartBase<C>> void registerInventoryPart(UpgradeContainerType<W, C> containerType, IUpgradeInventoryPartFactory<C, I> factory) {
		UPGRADE_INVENTORY_PARTS.put(containerType, factory);
	}

	public static <C extends UpgradeContainerBase<?, ?>> UpgradeSettingsTab<C> getTab(C container, Position position, BackpackScreen screen) {
		return getTabFactory(container).create(container, position, screen);
	}

	public static <C extends UpgradeContainerBase<?, ?>> Optional<UpgradeInventoryPartBase<C>> getInventoryPart(int upgradeSlot, C container, Position position, int height, BackpackScreen screen) {
		return getInventoryPartFactory(container).map(f -> f.create(upgradeSlot, container, position, height, screen));
	}

	@SuppressWarnings("unchecked")
	private static <C extends UpgradeContainerBase<?, ?>, S extends UpgradeSettingsTab<C>> IUpgradeSettingsFactory<C, S> getTabFactory(C container) {
		return (IUpgradeSettingsFactory<C, S>) getTabFactory(container.getType());
	}

	@SuppressWarnings("unchecked")
	private static <W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>, S extends UpgradeSettingsTab<C>> IUpgradeSettingsFactory<C, S> getTabFactory(UpgradeContainerType<W, C> containerType) {
		return (IUpgradeSettingsFactory<C, S>) UPGRADE_TABS.get(containerType);
	}

	@SuppressWarnings("unchecked")
	private static <C extends UpgradeContainerBase<?, ?>, I extends UpgradeInventoryPartBase<C>> Optional<IUpgradeInventoryPartFactory<C, I>> getInventoryPartFactory(C container) {
		if (!UPGRADE_INVENTORY_PARTS.containsKey(container.getType())) {
			return Optional.empty();
		}

		return Optional.of((IUpgradeInventoryPartFactory<C, I>) getInventoryPartFactory(container.getType()));
	}

	@SuppressWarnings("unchecked")
	private static <W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>, I extends UpgradeInventoryPartBase<C>> IUpgradeInventoryPartFactory<C, I> getInventoryPartFactory(UpgradeContainerType<W, C> containerType) {
		return (IUpgradeInventoryPartFactory<C, I>) UPGRADE_INVENTORY_PARTS.get(containerType);
	}

	public interface IUpgradeSettingsFactory<C extends UpgradeContainerBase<?, ?>, S extends UpgradeSettingsTab<C>> {
		S create(C container, Position position, BackpackScreen screen);
	}

	public interface IUpgradeInventoryPartFactory<C extends UpgradeContainerBase<?, ?>, I extends UpgradeInventoryPartBase<C>> {
		I create(int upgradeSlot, C container, Position position, int height, BackpackScreen screen);
	}
}
