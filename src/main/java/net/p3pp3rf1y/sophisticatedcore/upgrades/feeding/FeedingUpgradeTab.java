package net.p3pp3rf1y.sophisticatedcore.upgrades.feeding;

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
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControl;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.getBooleanStateData;

public class FeedingUpgradeTab extends UpgradeSettingsTab<FeedingUpgradeContainer> {
	public static final ButtonDefinition.Toggle<HungerLevel> HUNGER_LEVEL = createToggleButtonDefinition(
			Map.of(
					HungerLevel.ANY, GuiHelper.getButtonStateData(new UV(128, 0), TranslationHelper.INSTANCE.translUpgradeButton("hunger_level_any"), Dimension.SQUARE_16, new Position(1, 1)),
					HungerLevel.HALF, GuiHelper.getButtonStateData(new UV(112, 0), TranslationHelper.INSTANCE.translUpgradeButton("hunger_level_half"), Dimension.SQUARE_16, new Position(1, 1)),
					HungerLevel.FULL, GuiHelper.getButtonStateData(new UV(96, 0), TranslationHelper.INSTANCE.translUpgradeButton("hunger_level_full"), Dimension.SQUARE_16, new Position(1, 1))
			));

	public static final ButtonDefinition.Toggle<Boolean> FEED_IMMEDIATELY_WHEN_HURT = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(96, 16), TranslationHelper.INSTANCE.translUpgradeButton("feed_immediately_when_hurt"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(112, 16), TranslationHelper.INSTANCE.translUpgradeButton("do_not_consider_health"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	protected FeedingUpgradeTab(FeedingUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends FeedingUpgradeTab {
		public Basic(FeedingUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsPerRow) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("feeding"), TranslationHelper.INSTANCE.translUpgradeTooltip("feeding"));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(screen, new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					slotsPerRow));
		}
	}

	public static class Advanced extends FeedingUpgradeTab {
		public Advanced(FeedingUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsPerRow) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("advanced_feeding"), TranslationHelper.INSTANCE.translUpgradeTooltip("advanced_feeding"));
			addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), HUNGER_LEVEL,
					button -> getContainer().setFeedAtHungerLevel(getContainer().getFeedAtHungerLevel().next()),
					() -> getContainer().getFeedAtHungerLevel()));
			addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), FEED_IMMEDIATELY_WHEN_HURT,
					button -> getContainer().setFeedImmediatelyWhenHurt(!getContainer().shouldFeedImmediatelyWhenHurt()),
					() -> getContainer().shouldFeedImmediatelyWhenHurt()));

			filterLogicControl = addHideableChild(new FilterLogicControl.Advanced(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					slotsPerRow));
		}
	}
}
