package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilteredUpgradeContainer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

public class PickupUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<PickupUpgradeWrapper>> {
	protected FilterLogicControl filterLogicControl;

	protected PickupUpgradeTab(FilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends PickupUpgradeTab {
		public Basic(FilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("pickup")),
					new TranslationTextComponent(translUpgradeTooltip("pickup")));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.pickupUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends PickupUpgradeTab {
		public Advanced(FilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("advanced_pickup")),
					new TranslationTextComponent(translUpgradeTooltip("advanced_pickup")));
			filterLogicControl = addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedPickupUpgrade.slotsInRow.get()));
		}
	}
}
