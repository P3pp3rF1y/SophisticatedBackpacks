package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

import java.util.Map;

public class UpgradeSettingsTabControl extends SettingsTabControl<BackpackScreen, UpgradeSettingsTab<?>> {
	public UpgradeSettingsTabControl(Position position, BackpackScreen screen) {
		super(position);
		addChild(new BackpackSettingsTab(new Position(x, getTopY()), screen));
		for (Map.Entry<Integer, UpgradeContainerBase<?, ?>> entry : screen.getContainer().getUpgradeContainers().entrySet()) {
			addSettingsTab(() -> screen.getContainer().setOpenTabId(entry.getKey()), () -> screen.getContainer().removeOpenTabId(),
					UpgradeSettingsTabManager.getTab(entry.getValue(), new Position(x, getTopY()), screen))
					.onAfterInit();
		}
	}
}
