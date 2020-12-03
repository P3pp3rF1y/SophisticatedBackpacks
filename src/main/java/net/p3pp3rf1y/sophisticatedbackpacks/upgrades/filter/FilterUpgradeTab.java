package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;

@OnlyIn(Dist.CLIENT)
public abstract class FilterUpgradeTab extends UpgradeSettingsTab<FilterUpgradeContainer> {
	private static final ButtonDefinition.Toggle<Direction> DIRECTION = createToggleButtonDefinition(
			ImmutableMap.of(
					Direction.BOTH, GuiHelper.getButtonStateData(new UV(32, 64), translUpgradeButton("direction_both")),
					Direction.INPUT, GuiHelper.getButtonStateData(new UV(48, 64), translUpgradeButton("direction_input")),
					Direction.OUTPUT, GuiHelper.getButtonStateData(new UV(64, 64), translUpgradeButton("direction_output"))
			));

	protected FilterLogicControl filterLogicControl;

	protected FilterUpgradeTab(FilterUpgradeContainer upgradeContainer, Position position, Dimension openTabDimension, BackpackScreen screen,
			ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, openTabDimension, screen, tabLabel, closedTooltip);

		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), DIRECTION,
				button -> getContainer().setDirection(getContainer().getDirection().next()), () -> getContainer().getDirection()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends FilterUpgradeTab {
		public Basic(FilterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(63, 126), screen,
					new TranslationTextComponent(translUpgrade("filter")), new TranslationTextComponent(translUpgradeTooltip("filter")));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 44), getContainer(), 3));
		}
	}

	public static class Advanced extends FilterUpgradeTab {
		public Advanced(FilterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(81, 144), screen,
					new TranslationTextComponent(translUpgrade("advanced_filter")), new TranslationTextComponent(translUpgradeTooltip("advanced_filter")));

			filterLogicControl = addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 44), getContainer(), 4));
		}
	}
}
