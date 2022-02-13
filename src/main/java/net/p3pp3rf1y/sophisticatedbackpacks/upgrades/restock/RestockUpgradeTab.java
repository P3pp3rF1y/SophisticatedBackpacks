package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterControl;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilteredUpgradeContainer;

public abstract class RestockUpgradeTab extends UpgradeSettingsTab<ContentsFilteredUpgradeContainer<RestockUpgradeWrapper>> {
	protected ContentsFilterControl filterLogicControl;

	protected RestockUpgradeTab(ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> upgradeContainer, Position position, StorageScreenBase<?> screen, Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends RestockUpgradeTab {
		public Basic(ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> upgradeContainer, Position position, StorageScreenBase<?> screen, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("restock"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("restock"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.restockUpgrade.slotsInRow.get(), contentsFilterButton));
		}
	}

	public static class Advanced extends RestockUpgradeTab {
		public Advanced(ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> upgradeContainer, Position position, StorageScreenBase<?> screen, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("advanced_restock"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("advanced_restock"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedRestockUpgrade.slotsInRow.get(), contentsFilterButton));
		}
	}
}
