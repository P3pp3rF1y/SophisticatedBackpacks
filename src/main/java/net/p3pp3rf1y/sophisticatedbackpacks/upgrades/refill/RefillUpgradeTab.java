package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

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
public class RefillUpgradeTab extends UpgradeSettingsTab<FilteredUpgradeContainer<RefillUpgradeWrapper>> {
	private final FilterLogicControl filterLogicControl;

	public RefillUpgradeTab(FilteredUpgradeContainer<RefillUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, new Dimension(63, 67), screen, new TranslationTextComponent(translUpgrade("refill")), new TranslationTextComponent(translUpgradeTooltip("refill")));

		filterLogicControl = addHideableChild(new FilterLogicControl(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(), 3));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}
}
