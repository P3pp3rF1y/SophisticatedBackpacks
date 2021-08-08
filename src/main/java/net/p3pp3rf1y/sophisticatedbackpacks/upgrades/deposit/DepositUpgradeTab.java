package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class DepositUpgradeTab extends UpgradeSettingsTab<DepositUpgradeContainer> {
	protected DepositFilterLogicControl filterLogicControl;

	protected DepositUpgradeTab(DepositUpgradeContainer upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends DepositUpgradeTab {
		public Basic(DepositUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("deposit"), translUpgradeTooltip("deposit"));
			filterLogicControl = addHideableChild(new DepositFilterLogicControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.depositUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends DepositUpgradeTab {
		public Advanced(DepositUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_deposit"), translUpgradeTooltip("advanced_deposit"));
			filterLogicControl = addHideableChild(new DepositFilterLogicControl.Advanced(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedDepositUpgrade.slotsInRow.get()));
		}
	}
}
