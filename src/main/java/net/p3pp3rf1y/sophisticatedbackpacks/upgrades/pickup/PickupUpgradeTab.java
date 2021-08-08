package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilteredUpgradeContainer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class PickupUpgradeTab extends UpgradeSettingsTab<ContentsFilteredUpgradeContainer<PickupUpgradeWrapper>> {
	protected ContentsFilterControl filterLogicControl;

	protected PickupUpgradeTab(ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends PickupUpgradeTab {
		public Basic(ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("pickup"), translUpgradeTooltip("pickup"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.pickupUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends PickupUpgradeTab {
		public Advanced(ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_pickup"), translUpgradeTooltip("advanced_pickup"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedPickupUpgrade.slotsInRow.get()));
		}
	}
}
