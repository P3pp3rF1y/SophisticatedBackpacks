package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;

public class DepositUpgradeTab extends UpgradeSettingsTab<DepositUpgradeContainer> {
	protected DepositFilterLogicControl filterLogicControl;

	protected DepositUpgradeTab(DepositUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends DepositUpgradeTab {
		public Basic(DepositUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
			super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("deposit"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("deposit"));
			filterLogicControl = addHideableChild(new DepositFilterLogicControl.Basic(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.depositUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends DepositUpgradeTab {
		public Advanced(DepositUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
			super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("advanced_deposit"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("advanced_deposit"));
			filterLogicControl = addHideableChild(new DepositFilterLogicControl.Advanced(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedDepositUpgrade.slotsInRow.get()));
		}
	}
}
