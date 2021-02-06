package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;

public abstract class FilterUpgradeTab extends UpgradeSettingsTab<FilterUpgradeContainer> {
	private static final ButtonDefinition.Toggle<Direction> DIRECTION = createToggleButtonDefinition(
			ImmutableMap.of(
					Direction.BOTH, GuiHelper.getButtonStateData(new UV(32, 64), translUpgradeButton("direction_both"), Dimension.SQUARE_16, new Position(1, 1)),
					Direction.INPUT, GuiHelper.getButtonStateData(new UV(48, 64), translUpgradeButton("direction_input"), Dimension.SQUARE_16, new Position(1, 1)),
					Direction.OUTPUT, GuiHelper.getButtonStateData(new UV(64, 64), translUpgradeButton("direction_output"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	protected FilterUpgradeTab(FilterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen,
			ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);

		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), DIRECTION,
				button -> getContainer().setDirection(getContainer().getDirection().next()), () -> getContainer().getDirection()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends FilterUpgradeTab {
		public Basic(FilterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("filter")),
					new TranslationTextComponent(translUpgradeTooltip("filter")));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.filterUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends FilterUpgradeTab {
		public Advanced(FilterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("advanced_filter")),
					new TranslationTextComponent(translUpgradeTooltip("advanced_filter")));

			filterLogicControl = addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedFilterUpgrade.slotsInRow.get()));
		}
	}
}
