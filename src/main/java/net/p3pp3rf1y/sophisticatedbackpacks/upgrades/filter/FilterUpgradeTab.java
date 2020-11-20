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
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.*;

@OnlyIn(Dist.CLIENT)
public abstract class FilterUpgradeTab extends UpgradeSettingsTab<FilterUpgradeContainer> {
	protected FilterUpgradeTab(FilterUpgradeContainer upgradeContainer, Position position, Dimension openTabDimension, BackpackScreen screen) {
		super(upgradeContainer, position, openTabDimension, screen);

		ToggleButton<Direction> directionButton = addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), new Dimension(18, 18),
				button -> {
					getContainer().setDirection(getContainer().getDirection().next());
					return true;
				}, GuiHelper.DEFAULT_BUTTON_BACKGROUND,
				ImmutableMap.of(
						Direction.BOTH, GuiHelper.getButtonStateData(new UV(32, 64), translUpgradeButton("direction_both")),
						Direction.INPUT, GuiHelper.getButtonStateData(new UV(48, 64), translUpgradeButton("direction_input")),
						Direction.OUTPUT, GuiHelper.getButtonStateData(new UV(64, 64), translUpgradeButton("direction_output"))
				),
				() -> getContainer().getDirection()));
		directionButton.setHoveredBackgroundTexture(GuiHelper.DEFAULT_BUTTON_HOVERED_BACKGROUND);

		slotsTopY = y + 66;
	}

	public static class Basic extends FilterUpgradeTab {
		public Basic(FilterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(63, 126), screen);
			addHideableChild(new FilterLogicControl(new Position(x + 3, y + 44), getContainer()));
		}

		@Override
		protected int getSlotsPerRow() {
			return 3;
		}

		@Override
		protected ITextComponent getTabLabel() {
			return new TranslationTextComponent(translUpgrade("filter"));
		}

		@Override
		protected ITextComponent getClosedTooltip() {
			return new TranslationTextComponent(translUpgradeTooltip("filter"));
		}
	}

	public static class Advanced extends FilterUpgradeTab {
		public Advanced(FilterUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(81, 144), screen);

			addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 44), getContainer()));
		}

		@Override
		protected int getSlotsPerRow() {
			return 4;
		}

		@Override
		protected ITextComponent getTabLabel() {
			return new TranslationTextComponent(translUpgrade("advanced_filter"));
		}

		@Override
		protected ITextComponent getClosedTooltip() {
			return new TranslationTextComponent(translUpgradeTooltip("advanced_filter"));
		}
	}
}
