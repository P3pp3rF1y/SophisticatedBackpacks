package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

public class SlotSettingsTabControl extends SettingsTabControl {
	public SlotSettingsTabControl(Position position) {
		super(position);
		addChild(new BackToBackpackTab(new Position(x, getTopY())));
	}
}
