package net.p3pp3rf1y.sophisticatedcore.upgrades.filter;

import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterControl;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterType;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;

public abstract class FilterUpgradeTab extends UpgradeSettingsTab<FilterUpgradeContainer> {
	private static final ButtonDefinition.Toggle<Direction> DIRECTION = createToggleButtonDefinition(
			Map.of(
					Direction.BOTH, GuiHelper.getButtonStateData(new UV(0, 32), TranslationHelper.INSTANCE.translUpgradeButton("direction_both"), Dimension.SQUARE_16, new Position(1, 1)),
					Direction.INPUT, GuiHelper.getButtonStateData(new UV(16, 32), TranslationHelper.INSTANCE.translUpgradeButton("direction_input"), Dimension.SQUARE_16, new Position(1, 1)),
					Direction.OUTPUT, GuiHelper.getButtonStateData(new UV(32, 32), TranslationHelper.INSTANCE.translUpgradeButton("direction_output"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected ContentsFilterControl filterLogicControl;

	protected FilterUpgradeTab(FilterUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen,
			Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);

		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), DIRECTION,
				button -> getContainer().setDirection(getContainer().getDirection().next()), () -> getContainer().getDirection()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends FilterUpgradeTab {
		public Basic(FilterUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("filter"), TranslationHelper.INSTANCE.translUpgradeTooltip("filter"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					slotsPerRow, contentsFilterButton));
		}
	}

	public static class Advanced extends FilterUpgradeTab {
		public Advanced(FilterUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("advanced_filter"), TranslationHelper.INSTANCE.translUpgradeTooltip("advanced_filter"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					slotsPerRow, contentsFilterButton));
		}
	}
}
