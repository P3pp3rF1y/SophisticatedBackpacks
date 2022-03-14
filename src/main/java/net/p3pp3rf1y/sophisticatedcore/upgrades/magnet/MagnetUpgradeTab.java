package net.p3pp3rf1y.sophisticatedcore.upgrades.magnet;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
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

import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.getBooleanStateData;

public class MagnetUpgradeTab extends UpgradeSettingsTab<MagnetUpgradeContainer> {
	private static final ButtonDefinition.Toggle<Boolean> PICKUP_ITEMS = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(128, 48), TranslationHelper.INSTANCE.translUpgradeButton("pickup_items"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(144, 48), TranslationHelper.INSTANCE.translUpgradeButton("do_not_pickup_items"), Dimension.SQUARE_16, new Position(1, 1))
			));

	private static final ButtonDefinition.Toggle<Boolean> PICKUP_XP = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(96, 48), Dimension.SQUARE_16, new Position(1, 1), new TranslatableComponent(TranslationHelper.INSTANCE.translUpgradeButton("pickup_xp")), new TranslatableComponent(TranslationHelper.INSTANCE.translUpgradeButton("pickup_xp.detail")).withStyle(ChatFormatting.DARK_GRAY).withStyle(ChatFormatting.ITALIC)),
					GuiHelper.getButtonStateData(new UV(112, 48), TranslationHelper.INSTANCE.translUpgradeButton("do_not_pickup_xp"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected ContentsFilterControl filterLogicControl;

	protected MagnetUpgradeTab(MagnetUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);

		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), PICKUP_ITEMS,
				button -> getContainer().setPickupItems(!getContainer().shouldPickupItems()),
				() -> getContainer().shouldPickupItems()));
		addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), PICKUP_XP,
				button -> getContainer().setPickupXp(!getContainer().shouldPickupXp()),
				() -> getContainer().shouldPickupXp()));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	public static class Basic extends MagnetUpgradeTab {
		public Basic(MagnetUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("magnet"), TranslationHelper.INSTANCE.translUpgradeTooltip("magnet"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Basic(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					slotsPerRow, contentsFilterButton));

		}
	}

	public static class Advanced extends MagnetUpgradeTab {
		public Advanced(MagnetUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("advanced_magnet"), TranslationHelper.INSTANCE.translUpgradeTooltip("advanced_magnet"));
			filterLogicControl = addHideableChild(new ContentsFilterControl.Advanced(screen, new Position(x + 3, y + 44), getContainer().getFilterLogicContainer(),
					slotsPerRow, contentsFilterButton));
		}
	}
}
