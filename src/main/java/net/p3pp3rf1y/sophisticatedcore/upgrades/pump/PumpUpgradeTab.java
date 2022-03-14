package net.p3pp3rf1y.sophisticatedcore.upgrades.pump;

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

import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.getBooleanStateData;

public class PumpUpgradeTab extends UpgradeSettingsTab<PumpUpgradeContainer> {
	private static final ButtonDefinition.Toggle<Boolean> IS_INPUT = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(144, 0), TranslationHelper.INSTANCE.translUpgradeButton("pump_input"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(160, 0), TranslationHelper.INSTANCE.translUpgradeButton("pump_output"), Dimension.SQUARE_16, new Position(1, 1))
			));
	private static final ButtonDefinition.Toggle<Boolean> INTERACT_WITH_WORLD = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(176, 0), TranslationHelper.INSTANCE.translUpgradeButton("interact_with_world"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(192, 0), TranslationHelper.INSTANCE.translUpgradeButton("do_not_interact_with_world"), Dimension.SQUARE_16, new Position(1, 1))
			));
	private static final ButtonDefinition.Toggle<Boolean> INTERACT_WITH_HAND = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(208, 0), TranslationHelper.INSTANCE.translUpgradeButton("interact_with_hand"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(224, 0), TranslationHelper.INSTANCE.translUpgradeButton("do_not_interact_with_hand"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected PumpUpgradeTab(PumpUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, Component tabLabel, Component closedTooltip) {
		super(upgradeContainer, position, screen, tabLabel, closedTooltip);
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), IS_INPUT,
				button -> getContainer().setIsInput(!getContainer().isInput()),
				() -> getContainer().isInput()));
	}

	@Override
	protected void moveSlotsToTab() {
		//noop
	}

	public static class Basic extends PumpUpgradeTab {
		public Basic(PumpUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("pump"), TranslationHelper.INSTANCE.translUpgradeTooltip("pump"));
		}
	}

	public static class Advanced extends PumpUpgradeTab {
		public Advanced(PumpUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
			super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("advanced_pump"), TranslationHelper.INSTANCE.translUpgradeTooltip("advanced_pump"));
			addHideableChild(new ToggleButton<>(new Position(x + 21, y + 24), INTERACT_WITH_WORLD,
					button -> getContainer().setInteractWithWorld(!getContainer().shouldInteractWithWorld()),
					() -> getContainer().shouldInteractWithWorld()));
			addHideableChild(new ToggleButton<>(new Position(x + 39, y + 24), INTERACT_WITH_HAND,
					button -> getContainer().setInteractWithHand(!getContainer().shouldInteractWithHand()),
					() -> getContainer().shouldInteractWithHand()));
			addHideableChild(new FluidFilterControl(new Position(x + 3, y + 44), getContainer().getFluidFilterContainer()));
		}
	}
}
