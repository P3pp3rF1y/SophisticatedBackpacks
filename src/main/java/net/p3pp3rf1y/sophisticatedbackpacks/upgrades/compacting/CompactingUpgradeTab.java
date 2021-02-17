package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting;

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
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl.Button.ALLOW_LIST;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl.Button.PRIMARY_MATCH;

public abstract class CompactingUpgradeTab extends UpgradeSettingsTab<CompactingUpgradeContainer> {
	public static final ButtonDefinition.Toggle<Boolean> COMPACT_UNCRAFTABLE = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					false, GuiHelper.getButtonStateData(new UV(112, 80), translUpgradeButton("compact_only_uncraftable"), Dimension.SQUARE_16, new Position(1, 1)),
					true, GuiHelper.getButtonStateData(new UV(112, 64), translUpgradeButton("compact_anything"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	protected CompactingUpgradeTab(CompactingUpgradeContainer container, Position position,
			BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(container, position, screen, tabLabel, closedTooltip);
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), COMPACT_UNCRAFTABLE, button -> getContainer().setCompactNonUncraftable(!getContainer().shouldCompactNonUncraftable()),
				getContainer()::shouldCompactNonUncraftable));
		addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), ButtonDefinitions.WORK_IN_GUI, button -> getContainer().setShouldWorkdInGUI(!getContainer().shouldWorkInGUI()),
				getContainer()::shouldWorkInGUI));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends CompactingUpgradeTab {
		public Basic(CompactingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("compacting")),
					new TranslationTextComponent(translUpgradeTooltip("compacting")));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.compactingUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends CompactingUpgradeTab {
		public Advanced(CompactingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("advanced_compacting")),
					new TranslationTextComponent(translUpgradeTooltip("advanced_compacting")));
			filterLogicControl = addHideableChild(new FilterLogicControl<>(new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedCompactingUpgrade.slotsInRow.get(), ALLOW_LIST, PRIMARY_MATCH));
		}
	}
}
