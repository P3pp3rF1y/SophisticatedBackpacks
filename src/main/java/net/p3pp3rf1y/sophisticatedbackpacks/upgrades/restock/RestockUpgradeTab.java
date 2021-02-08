package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

public abstract class RestockUpgradeTab extends UpgradeSettingsTab<RestockUpgradeContainer> {
	protected RestockFilterLogicControl filterLogicControl;

	protected RestockUpgradeTab(RestockUpgradeContainer upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends RestockUpgradeTab {
		public Basic(RestockUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("restock")),
					new TranslationTextComponent(translUpgradeTooltip("restock")));
			filterLogicControl = addHideableChild(new RestockFilterLogicControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.restockUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends RestockUpgradeTab {
		public Advanced(RestockUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("advanced_restock")),
					new TranslationTextComponent(translUpgradeTooltip("advanced_restock")));
			filterLogicControl = addHideableChild(new RestockFilterLogicControl.Advanced(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedRestockUpgrade.slotsInRow.get()));
		}
	}
}
