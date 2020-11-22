package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

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
public class FeedingUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<FeedingUpgradeWrapper>> {
	public FeedingUpgradeTab(FilteredUpgradeContainer<FeedingUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, new Dimension(63, 106), screen, 3,
				new TranslationTextComponent(translUpgrade("feeding")), new TranslationTextComponent(translUpgradeTooltip("feeding")));
		addHideableChild(new FilterLogicControl(new Position(x + 3, y + 24), getContainer()));
	}
}
