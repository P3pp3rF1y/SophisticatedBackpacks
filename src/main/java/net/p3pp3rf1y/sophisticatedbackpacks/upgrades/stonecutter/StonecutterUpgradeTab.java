package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter;

import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class StonecutterUpgradeTab extends UpgradeSettingsTab<StonecutterUpgradeContainer> {
	private final StonecutterRecipeControl recipeControl;

	public StonecutterUpgradeTab(StonecutterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, translUpgrade("stonecutter"), translUpgradeTooltip("stonecutter"));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), ButtonDefinitions.SHIFT_CLICK_TARGET, button -> getContainer().setShiftClickIntoBackpack(!getContainer().shouldShiftClickIntoBackpack()),
				getContainer()::shouldShiftClickIntoBackpack));
		recipeControl = new StonecutterRecipeControl(screen, upgradeContainer.getRecipeContainer(), new Position(x + 3, y + 24));
		addHideableChild(recipeControl);
	}

	@Override
	protected void moveSlotsToTab() {
		recipeControl.moveSlotsToView();
	}
}
