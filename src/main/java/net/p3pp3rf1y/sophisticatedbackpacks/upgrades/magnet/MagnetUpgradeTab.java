package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilteredUpgradeContainer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class MagnetUpgradeTab extends UpgradeSettingsTab<ContentsFilteredUpgradeContainer<MagnetUpgradeWrapper>> {
	protected ContentsFilterControl filterLogicControl;

	protected MagnetUpgradeTab(ContentsFilteredUpgradeContainer<MagnetUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends MagnetUpgradeTab {
		public Basic(ContentsFilteredUpgradeContainer<MagnetUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("magnet"), translUpgradeTooltip("magnet"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.magnetUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends MagnetUpgradeTab {
		public Advanced(ContentsFilteredUpgradeContainer<MagnetUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_magnet"), translUpgradeTooltip("advanced_magnet"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedMagnetUpgrade.slotsInRow.get()));
		}
	}
}
