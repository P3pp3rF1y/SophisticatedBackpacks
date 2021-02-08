package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilteredUpgradeContainer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

public class RefillUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<RefillUpgradeWrapper>> {
	private final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	public RefillUpgradeTab(FilteredUpgradeContainer<RefillUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("refill")), new TranslationTextComponent(translUpgradeTooltip("refill")));

		filterLogicControl = addHideableChild(new FilterLogicControl<>(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
				Config.COMMON.refillUpgrade.slotsInRow.get()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}
}
