package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.getButtonStateData;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.*;

public class VoidUpgradeTab extends UpgradeSettingsTab<VoidUpgradeContainer> {
	private static final ButtonDefinition.Toggle<Boolean> VOID_OVERFLOW = ButtonDefinitions.createToggleButtonDefinition(
			Map.of(
					true, getButtonStateData(new UV(224, 16), Dimension.SQUARE_16, new Position(1, 1), new TranslatableComponent(translUpgradeButton("void_overflow"))
							, new TranslatableComponent(translUpgradeButton("void_overflow.detail")).withStyle(ChatFormatting.GRAY)),
					false, getButtonStateData(new UV(208, 16), translUpgradeButton("void_any"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	protected VoidUpgradeTab(VoidUpgradeContainer upgradeContainer, Position position, BackpackScreen screen, Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), ButtonDefinitions.WORK_IN_GUI, button -> getContainer().setShouldWorkdInGUI(!getContainer().shouldWorkInGUI()),
				getContainer()::shouldWorkInGUI));
		addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), VOID_OVERFLOW, button -> getContainer().setShouldVoidOverflow(!getContainer().shouldVoidOverflow()),
				getContainer()::shouldVoidOverflow));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends VoidUpgradeTab {
		public Basic(VoidUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("void"), translUpgradeTooltip("void"));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.voidUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends VoidUpgradeTab {
		public Advanced(VoidUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_void"), translUpgradeTooltip("advanced_void"));
			filterLogicControl = addHideableChild(new FilterLogicControl.Advanced(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedVoidUpgrade.slotsInRow.get()));
		}
	}
}
