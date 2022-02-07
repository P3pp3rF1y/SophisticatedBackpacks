package net.p3pp3rf1y.sophisticatedcore.upgrades.pickup;

import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterControl;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilteredUpgradeContainer;

public class PickupUpgradeTab extends UpgradeSettingsTab<ContentsFilteredUpgradeContainer<PickupUpgradeWrapper>> {
	protected ContentsFilterControl filterLogicControl;

	protected PickupUpgradeTab(ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, StorageScreen<?> screen, Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends PickupUpgradeTab {
		public Basic(ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, StorageScreen<?> screen, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("pickup"), TranslationHelper.INSTANCE.translUpgradeTooltip("pickup"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					slotsPerRow, contentsFilterButton));
		}
	}

	public static class Advanced extends PickupUpgradeTab {
		public Advanced(ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> upgradeContainer, Position position, StorageScreen<?> screen, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("advanced_pickup"), TranslationHelper.INSTANCE.translUpgradeTooltip("advanced_pickup"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					slotsPerRow, contentsFilterButton));
		}
	}
}
