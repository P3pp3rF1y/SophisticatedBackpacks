package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControl;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilteredUpgradeContainer;

public class RefillUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<RefillUpgradeWrapper>> {
	private final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	public RefillUpgradeTab(FilteredUpgradeContainer<RefillUpgradeWrapper> upgradeContainer, Position position, StorageScreen<?> screen) {
		super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("refill"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("refill"));

		filterLogicControl = addHideableChild(new FilterLogicControl<>(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
				Config.COMMON.refillUpgrade.slotsInRow.get()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}
}
