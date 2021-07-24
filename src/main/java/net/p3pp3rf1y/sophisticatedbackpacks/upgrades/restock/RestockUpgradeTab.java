package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilteredUpgradeContainer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public abstract class RestockUpgradeTab extends UpgradeSettingsTab<ContentsFilteredUpgradeContainer<RestockUpgradeWrapper>> {
	protected ContentsFilterControl filterLogicControl;

	protected RestockUpgradeTab(ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends RestockUpgradeTab {
		public Basic(ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("restock"), translUpgradeTooltip("restock"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.restockUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends RestockUpgradeTab {
		public Advanced(ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_restock"), translUpgradeTooltip("advanced_restock"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedRestockUpgrade.slotsInRow.get()));
		}
	}
}
