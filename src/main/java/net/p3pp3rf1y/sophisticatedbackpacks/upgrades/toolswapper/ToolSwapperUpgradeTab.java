package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControl;

import java.util.Map;

public class ToolSwapperUpgradeTab extends UpgradeSettingsTab<ToolSwapperUpgradeContainer> {
	public static final ButtonDefinition.Toggle<Boolean> SWAP_WEAPON = ButtonDefinitions.createToggleButtonDefinition(
			Map.of(
					false, GuiHelper.getButtonStateData(new UV(48, 64), Dimension.SQUARE_16, new Position(1, 1),
							Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("do_not_swap_weapon")), Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("do_not_swap_weapon.detail")).withStyle(ChatFormatting.GRAY)),
					true, GuiHelper.getButtonStateData(new UV(32, 64), Dimension.SQUARE_16, new Position(1, 1),
							Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("swap_weapon")), Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("swap_weapon.detail")).withStyle(ChatFormatting.GRAY))
			));

	public static final ButtonDefinition.Toggle<ToolSwapMode> SWAP_TOOLS = ButtonDefinitions.createToggleButtonDefinition(
			Map.of(
					ToolSwapMode.NO_SWAP, GuiHelper.getButtonStateData(new UV(96, 64), Dimension.SQUARE_16, new Position(1, 1),
							Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("do_not_swap_tools")), Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("do_not_swap_tools.detail")).withStyle(ChatFormatting.GRAY)),
					ToolSwapMode.ONLY_TOOLS, GuiHelper.getButtonStateData(new UV(80, 64), Dimension.SQUARE_16, new Position(1, 1),
							Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("only_swap_for_tools")), Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("only_swap_for_tools.detail")).withStyle(ChatFormatting.GRAY)),
					ToolSwapMode.ANY, GuiHelper.getButtonStateData(new UV(64, 64), Dimension.SQUARE_16, new Position(1, 1),
							Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("swap_tools")), Component.translatable(SBPTranslationHelper.INSTANCE.translUpgradeButton("swap_tools.detail")).withStyle(ChatFormatting.GRAY))
			));

	protected final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	public ToolSwapperUpgradeTab(ToolSwapperUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
		super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("advanced_tool_swapper"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("advanced_tool_swapper"));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), SWAP_WEAPON, button -> getContainer().setSwapWeapon(!getContainer().shouldSwapWeapon()),
				getContainer()::shouldSwapWeapon));
		addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), SWAP_TOOLS, button -> getContainer().setToolSwapMode(getContainer().getToolSwapMode().next()),
				getContainer()::getToolSwapMode));

		filterLogicControl = addHideableChild(new FilterLogicControl.Advanced(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(), 4));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}
}
