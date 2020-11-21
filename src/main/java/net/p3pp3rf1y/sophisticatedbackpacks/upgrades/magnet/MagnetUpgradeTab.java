package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilteredUpgradeContainer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

@OnlyIn(Dist.CLIENT)
public class MagnetUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<MagnetUpgradeWrapper>> {
	protected MagnetUpgradeTab(FilteredUpgradeContainer<MagnetUpgradeWrapper> upgradeContainer, Position position, Dimension openTabDimension, BackpackScreen screen, int slotsPerRow, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, openTabDimension, screen, slotsPerRow, tabLabel, closedTooltip);
	}

	public static class Basic extends MagnetUpgradeTab {
		public Basic(FilteredUpgradeContainer<MagnetUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(63, 106), screen, 3,
					new TranslationTextComponent(translUpgrade("magnet")), new TranslationTextComponent(translUpgradeTooltip("magnet")));
			addHideableChild(new FilterLogicControl(new Position(x + 3, y + 24), getContainer()));
		}
	}

	public static class Advanced extends MagnetUpgradeTab {
		public Advanced(FilteredUpgradeContainer<MagnetUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(81, 124), screen, 4,
					new TranslationTextComponent(translUpgrade("advanced_magnet")), new TranslationTextComponent(translUpgradeTooltip("advanced_magnet")));
			addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 24), getContainer()));
		}
	}
}
