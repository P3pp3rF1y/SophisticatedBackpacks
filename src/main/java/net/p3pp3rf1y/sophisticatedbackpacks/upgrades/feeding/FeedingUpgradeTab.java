package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilteredUpgradeContainer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class FeedingUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<FeedingUpgradeWrapper>> {
	private final FilterLogicControl.Basic filterLogicControl;

	public FeedingUpgradeTab(FilteredUpgradeContainer<FeedingUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, translUpgrade("feeding"), translUpgradeTooltip("feeding"));
		filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(), Config.COMMON.feedingUpgrade.slotsInRow.get()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}
}
