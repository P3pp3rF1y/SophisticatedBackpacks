package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.google.common.collect.ImmutableMap;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.PrimaryMatch;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper.DEFAULT_BUTTON_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper.DEFAULT_BUTTON_HOVERED_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeButton;

public class ButtonDefinitions {
	private ButtonDefinitions() {}

	private static Map<Boolean, ToggleButton.StateData> getBooleanStateData(ToggleButton.StateData onStateData, ToggleButton.StateData offStateData) {
		return ImmutableMap.of(
				true, onStateData,
				false, offStateData
		);
	}

	public static <T extends Comparable<T>> ButtonDefinition.Toggle<T> createToggleButtonDefinition(Map<T, ToggleButton.StateData> stateData) {
		return new ButtonDefinition.Toggle<>(Dimension.SQUARE_18, DEFAULT_BUTTON_BACKGROUND, stateData, DEFAULT_BUTTON_HOVERED_BACKGROUND);
	}

	public static final ButtonDefinition.Toggle<Boolean> ALLOW_LIST = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(32, 32), translUpgradeButton("allow")),
					GuiHelper.getButtonStateData(new UV(48, 32), translUpgradeButton("block"))
			));

	public static final ButtonDefinition.Toggle<Boolean> MATCH_DURABILITY = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(32, 48), translUpgradeButton("match_durability")),
					GuiHelper.getButtonStateData(new UV(48, 48), translUpgradeButton("ignore_durability"))
			));

	public static final ButtonDefinition.Toggle<Boolean> MATCH_NBT = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(64, 32), translUpgradeButton("match_nbt")),
					GuiHelper.getButtonStateData(new UV(80, 32), translUpgradeButton("ignore_nbt"))
			));

	public static final ButtonDefinition.Toggle<PrimaryMatch> PRIMARY_MATCH = createToggleButtonDefinition(
			ImmutableMap.of(
					PrimaryMatch.ITEM, GuiHelper.getButtonStateData(new UV(80, 48), translUpgradeButton("match_item")),
					PrimaryMatch.MOD, GuiHelper.getButtonStateData(new UV(64, 48), translUpgradeButton("match_mod")),
					PrimaryMatch.TAGS, GuiHelper.getButtonStateData(new UV(96, 32), translUpgradeButton("match_tags"))
			));
}
