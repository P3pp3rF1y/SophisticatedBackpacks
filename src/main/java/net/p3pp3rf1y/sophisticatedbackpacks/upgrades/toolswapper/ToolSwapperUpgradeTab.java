package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.*;

public class ToolSwapperUpgradeTab extends UpgradeSettingsTab<ToolSwapperUpgradeContainer> {
	protected ToolSwapperFilterControl filterLogicControl;

	public ToolSwapperUpgradeTab(ToolSwapperUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("advanced_tool_swapper")),
				new TranslationTextComponent(translUpgradeTooltip("advanced_tool_swapper")));
		filterLogicControl = addHideableChild(new ToolSwapperFilterControl(getContainer().getFilterLogicContainer(), new Position(x + 3, y + 24), true,
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
