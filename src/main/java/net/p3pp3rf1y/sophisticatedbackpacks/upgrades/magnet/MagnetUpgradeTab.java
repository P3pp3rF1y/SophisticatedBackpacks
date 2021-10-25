package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.getBooleanStateData;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.*;

public class MagnetUpgradeTab extends UpgradeSettingsTab<MagnetUpgradeContainer> {
	private static final ButtonDefinition.Toggle<Boolean> PICKUP_ITEMS = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(128, 48), translUpgradeButton("pickup_items"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(144, 48), translUpgradeButton("do_not_pickup_items"), Dimension.SQUARE_16, new Position(1, 1))
			));

	private static final ButtonDefinition.Toggle<Boolean> PICKUP_XP = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(96, 48), Dimension.SQUARE_16, new Position(1, 1), new TranslationTextComponent(translUpgradeButton("pickup_xp")), new TranslationTextComponent(translUpgradeButton("pickup_xp.detail")).withStyle(TextFormatting.DARK_GRAY).withStyle(TextFormatting.ITALIC)),
					GuiHelper.getButtonStateData(new UV(112, 48), translUpgradeButton("do_not_pickup_xp"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected ContentsFilterControl filterLogicControl;

	protected MagnetUpgradeTab(MagnetUpgradeContainer upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);

		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), PICKUP_ITEMS,
				button -> getContainer().setPickupItems(!getContainer().shouldPickupItems()),
				() -> getContainer().shouldPickupItems()));
		addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), PICKUP_XP,
				button -> getContainer().setPickupXp(!getContainer().shouldPickupXp()),
				() -> getContainer().shouldPickupXp()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends MagnetUpgradeTab {
		public Basic(MagnetUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("magnet"), translUpgradeTooltip("magnet"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.magnetUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends MagnetUpgradeTab {
		public Advanced(MagnetUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_magnet"), translUpgradeTooltip("advanced_magnet"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedMagnetUpgrade.slotsInRow.get()));
		}
	}
}
