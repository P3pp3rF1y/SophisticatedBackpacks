package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.ALLOW_LIST;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.PRIMARY_MATCH;

public abstract class CompactingUpgradeTab extends UpgradeSettingsTab<CompactingUpgradeContainer> {
	public static final ButtonDefinition.Toggle<Boolean> COMPACT_UNCRAFTABLE = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					false, GuiHelper.getButtonStateData(new UV(80, 48), translUpgradeButton("compact_only_uncraftable"), Dimension.SQUARE_16, new Position(1, 1)),
					true, GuiHelper.getButtonStateData(new UV(80, 32), translUpgradeButton("compact_anything"), Dimension.SQUARE_16, new Position(1, 1))
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
			super(upgradeContainer, position, screen, translUpgrade("compacting"), translUpgradeTooltip("compacting"));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.compactingUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends CompactingUpgradeTab {
		public Advanced(CompactingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_compacting"), translUpgradeTooltip("advanced_compacting"));
			filterLogicControl = addHideableChild(new FilterLogicControl<>(new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedCompactingUpgrade.slotsInRow.get(), ALLOW_LIST, PRIMARY_MATCH));
		}
	}
}
