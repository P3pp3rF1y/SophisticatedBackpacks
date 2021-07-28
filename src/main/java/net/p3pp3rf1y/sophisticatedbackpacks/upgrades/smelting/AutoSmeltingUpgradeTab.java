package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class AutoSmeltingUpgradeTab extends UpgradeSettingsTab<AutoSmeltingUpgradeContainer> {
	private final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> inputFilterLogicControl;
	private final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> fuelFilterLogicControl;
	private final SmeltingLogicControl smeltingLogicControl;

	public AutoSmeltingUpgradeTab(AutoSmeltingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, translUpgrade("auto_smelting"), translUpgradeTooltip("auto_smelting"));
		inputFilterLogicControl = addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 24), getContainer().getInputFilterLogicContainer(),
				Config.COMMON.autoSmeltingUpgrade.inputFilterSlotsInRow.get()));
		smeltingLogicControl = addHideableChild(new SmeltingLogicControl(new Position(x + 3, y + 84), getContainer().getSmeltingLogicContainer()));
		fuelFilterLogicControl = addHideableChild(new FilterLogicControl<>(new Position(x + 3, y + 142), getContainer().getFuelFilterLogicContainer(),
				Config.COMMON.autoSmeltingUpgrade.fuelFilterSlotsInRow.get()));
	}

	@Override
	protected void moveSlotsToTab() {
		inputFilterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
		smeltingLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
		fuelFilterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}
}
