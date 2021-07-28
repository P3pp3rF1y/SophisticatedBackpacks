package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class SmeltingUpgradeTab extends UpgradeSettingsTab<SmeltingUpgradeContainer> {
	private final SmeltingLogicControl smeltingLogicControl;

	public SmeltingUpgradeTab(SmeltingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, translUpgrade("smelting"), translUpgradeTooltip("smelting"));
		smeltingLogicControl = addHideableChild(new SmeltingLogicControl(new Position(x + 3, y + 24), getContainer().getSmeltingLogicContainer()));
	}

	@Override
	protected void moveSlotsToTab() {
		smeltingLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}
}
