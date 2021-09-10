package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
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
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.*;

public class ToolSwapperUpgradeTab extends UpgradeSettingsTab<ToolSwapperUpgradeContainer> {
	public static final ButtonDefinition.Toggle<Boolean> SWAP_WEAPON = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					false, GuiHelper.getButtonStateData(new UV(48, 64), Dimension.SQUARE_16, new Position(1, 1),
							new TranslationTextComponent(translUpgradeButton("do_not_swap_weapon")), new TranslationTextComponent(translUpgradeButton("do_not_swap_weapon.detail")).withStyle(TextFormatting.GRAY)),
					true, GuiHelper.getButtonStateData(new UV(32, 64), Dimension.SQUARE_16, new Position(1, 1),
							new TranslationTextComponent(translUpgradeButton("swap_weapon")), new TranslationTextComponent(translUpgradeButton("swap_weapon.detail")).withStyle(TextFormatting.GRAY))
			));

	public static final ButtonDefinition.Toggle<ToolSwapMode> SWAP_TOOLS = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					ToolSwapMode.NO_SWAP, GuiHelper.getButtonStateData(new UV(96, 64), Dimension.SQUARE_16, new Position(1, 1),
							new TranslationTextComponent(translUpgradeButton("do_not_swap_tools")), new TranslationTextComponent(translUpgradeButton("do_not_swap_tools.detail")).withStyle(TextFormatting.GRAY)),
					ToolSwapMode.ONLY_TOOLS, GuiHelper.getButtonStateData(new UV(80, 64), Dimension.SQUARE_16, new Position(1, 1),
							new TranslationTextComponent(translUpgradeButton("only_swap_for_tools")), new TranslationTextComponent(translUpgradeButton("only_swap_for_tools.detail")).withStyle(TextFormatting.GRAY)),
					ToolSwapMode.ANY, GuiHelper.getButtonStateData(new UV(64, 64), Dimension.SQUARE_16, new Position(1, 1),
							new TranslationTextComponent(translUpgradeButton("swap_tools")), new TranslationTextComponent(translUpgradeButton("swap_tools.detail")).withStyle(TextFormatting.GRAY))
			));

	protected ToolSwapperFilterControl filterLogicControl;

	public ToolSwapperUpgradeTab(ToolSwapperUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, translUpgrade("advanced_tool_swapper"), translUpgradeTooltip("advanced_tool_swapper"));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), SWAP_WEAPON, button -> getContainer().setSwapWeapon(!getContainer().shouldSwapWeapon()),
				getContainer()::shouldSwapWeapon));
		addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), SWAP_TOOLS, button -> getContainer().setToolSwapMode(getContainer().getToolSwapMode().next()),
				getContainer()::getToolSwapMode));

		filterLogicControl = addHideableChild(new ToolSwapperFilterControl(getContainer().getFilterLogicContainer(), new Position(x + 3, y + 44), true,
				Config.COMMON.toolSwapperUpgrade.slotsInRow.get()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class ToolSwapperFilterControl
			extends FilterLogicControlBase<ToolSwapperFilterLogic, ToolFilterSlot, FilterLogicContainerBase<ToolSwapperFilterLogic, ToolFilterSlot>> {
		protected ToolSwapperFilterControl(FilterLogicContainerBase<ToolSwapperFilterLogic, ToolFilterSlot> container, Position position, boolean buttonsVisible, int slotsPerRow) {
			super(container, position, buttonsVisible, slotsPerRow, ALLOW_LIST, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
