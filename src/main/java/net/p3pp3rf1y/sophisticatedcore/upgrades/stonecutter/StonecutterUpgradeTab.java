package net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter;

import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;

public class StonecutterUpgradeTab extends UpgradeSettingsTab<StonecutterUpgradeContainer> {
	private final StonecutterRecipeControl recipeControl;

	public StonecutterUpgradeTab(StonecutterUpgradeContainer upgradeContainer, Position position, StorageScreen<?> screen, ButtonDefinition.Toggle<Boolean> shiftClickTargetButton) {
		super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("stonecutter"), TranslationHelper.INSTANCE.translUpgradeTooltip("stonecutter"));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), shiftClickTargetButton, button -> getContainer().setShiftClickIntoStorage(!getContainer().shouldShiftClickIntoStorage()),
				getContainer()::shouldShiftClickIntoStorage));
		recipeControl = new StonecutterRecipeControl(screen, upgradeContainer.getRecipeContainer(), new Position(x + 3, y + 24));
		addHideableChild(recipeControl);
	}

	@Override
	protected void moveSlotsToTab() {
		recipeControl.moveSlotsToView();
	}
}
