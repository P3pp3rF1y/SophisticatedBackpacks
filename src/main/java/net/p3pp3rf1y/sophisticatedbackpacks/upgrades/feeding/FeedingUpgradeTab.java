package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.getBooleanStateData;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.*;

public class FeedingUpgradeTab extends UpgradeSettingsTab<FeedingUpgradeContainer> {
	public static final ButtonDefinition.Toggle<HungerLevel> HUNGER_LEVEL = createToggleButtonDefinition(
			ImmutableMap.of(
					HungerLevel.ANY, GuiHelper.getButtonStateData(new UV(128, 0), translUpgradeButton("hunger_level_any"), Dimension.SQUARE_16, new Position(1, 1)),
					HungerLevel.HALF, GuiHelper.getButtonStateData(new UV(112, 0), translUpgradeButton("hunger_level_half"), Dimension.SQUARE_16, new Position(1, 1)),
					HungerLevel.FULL, GuiHelper.getButtonStateData(new UV(96, 0), translUpgradeButton("hunger_level_full"), Dimension.SQUARE_16, new Position(1, 1))
			));

	public static final ButtonDefinition.Toggle<Boolean> FEED_IMMEDIATELY_WHEN_HURT = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(96, 16), translUpgradeButton("feed_immediately_when_hurt"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(112, 16), translUpgradeButton("do_not_consider_health"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;

	protected FeedingUpgradeTab(FeedingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class Basic extends FeedingUpgradeTab {
		public Basic(FeedingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("feeding"), translUpgradeTooltip("feeding"));
			filterLogicControl = addHideableChild(new FilterLogicControl.Basic(new Position(x + 3, y + 24), getContainer().getFilterLogicContainer(),
					Config.COMMON.restockUpgrade.slotsInRow.get()));
		}
	}

	public static class Advanced extends FeedingUpgradeTab {
		public Advanced(FeedingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, translUpgrade("advanced_feeding"), translUpgradeTooltip("advanced_feeding"));
			addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), HUNGER_LEVEL,
					button -> getContainer().setFeedAtHungerLevel(getContainer().getFeedAtHungerLevel().next()),
					() -> getContainer().getFeedAtHungerLevel()));
			addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), FEED_IMMEDIATELY_WHEN_HURT,
					button -> getContainer().setFeedImmediatelyWhenHurt(!getContainer().shouldFeedImmediatelyWhenHurt()),
					() -> getContainer().shouldFeedImmediatelyWhenHurt()));

			filterLogicControl = addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					Config.COMMON.advancedRestockUpgrade.slotsInRow.get()));
		}
	}
}
