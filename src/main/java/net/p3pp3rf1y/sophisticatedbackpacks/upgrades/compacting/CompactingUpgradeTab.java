package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting;

import net.minecraft.util.text.ITextComponent;
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
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl.Button.ALLOW_LIST;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl.Button.PRIMARY_MATCH;

public abstract class CompactingUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<CompactingUpgradeWrapper>> {
	protected FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	protected CompactingUpgradeTab(FilteredUpgradeContainer<CompactingUpgradeWrapper> upgradeContainer, Position position,
			BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends CompactingUpgradeTab {
		public Basic(FilteredUpgradeContainer<CompactingUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("compacting")),
					new TranslationTextComponent(translUpgradeTooltip("compacting")));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.compactingUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends CompactingUpgradeTab {
		public Advanced(FilteredUpgradeContainer<CompactingUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("advanced_compacting")),
					new TranslationTextComponent(translUpgradeTooltip("advanced_compacting")));
			filterLogicControl = addHideableChild(new FilterLogicControl<>(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedCompactingUpgrade.slotsInRow.get(), ALLOW_LIST, PRIMARY_MATCH));
		}
	}
}
