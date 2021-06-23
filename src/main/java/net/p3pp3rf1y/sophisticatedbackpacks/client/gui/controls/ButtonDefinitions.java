package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.PrimaryMatch;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeButton;

public class ButtonDefinitions {
	private ButtonDefinitions() {}

	public static Map<Boolean, ToggleButton.StateData> getBooleanStateData(ToggleButton.StateData onStateData, ToggleButton.StateData offStateData) {
		return ImmutableMap.of(
				true, onStateData,
				false, offStateData
		);
	}

	public static <T extends Comparable<T>> ButtonDefinition.Toggle<T> createToggleButtonDefinition(Map<T, ToggleButton.StateData> stateData) {
		return new ButtonDefinition.Toggle<>(Dimension.SQUARE_18, DEFAULT_BUTTON_BACKGROUND, stateData, DEFAULT_BUTTON_HOVERED_BACKGROUND);
	}

	public static <T extends Comparable<T>> ButtonDefinition.Toggle<T> createSmallToggleButtonDefinition(Map<T, ToggleButton.StateData> stateData) {
		return new ButtonDefinition.Toggle<>(Dimension.SQUARE_12, SMALL_BUTTON_BACKGROUND, stateData, SMALL_BUTTON_HOVERED_BACKGROUND);
	}

	public static final ButtonDefinition.Toggle<Boolean> ALLOW_LIST = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(0, 0), translUpgradeButton("allow"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(16, 0), translUpgradeButton("block"), Dimension.SQUARE_16, new Position(1, 1))
			));

	public static final ButtonDefinition.Toggle<Boolean> MATCH_DURABILITY = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(0, 16), translUpgradeButton("match_durability"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(16, 16), translUpgradeButton("ignore_durability"), Dimension.SQUARE_16, new Position(1, 1))
			));

	public static final ButtonDefinition.Toggle<Boolean> MATCH_NBT = createToggleButtonDefinition(
			getBooleanStateData(
					GuiHelper.getButtonStateData(new UV(32, 0), translUpgradeButton("match_nbt"), Dimension.SQUARE_16, new Position(1, 1)),
					GuiHelper.getButtonStateData(new UV(48, 0), translUpgradeButton("ignore_nbt"), Dimension.SQUARE_16, new Position(1, 1))
			));

	public static final ButtonDefinition.Toggle<PrimaryMatch> PRIMARY_MATCH = createToggleButtonDefinition(
			ImmutableMap.of(
					PrimaryMatch.ITEM, GuiHelper.getButtonStateData(new UV(48, 16), translUpgradeButton("match_item"), Dimension.SQUARE_16, new Position(1, 1)),
					PrimaryMatch.MOD, GuiHelper.getButtonStateData(new UV(32, 16), translUpgradeButton("match_mod"), Dimension.SQUARE_16, new Position(1, 1)),
					PrimaryMatch.TAGS, GuiHelper.getButtonStateData(new UV(64, 0), translUpgradeButton("match_tags"), Dimension.SQUARE_16, new Position(1, 1))
			));

	public static final ButtonDefinition.Toggle<SortBy> SORT_BY = createSmallToggleButtonDefinition(
			ImmutableMap.of(
					SortBy.NAME, GuiHelper.getButtonStateData(new UV(24, 144), TranslationHelper.translButton("sort_by_name"), Dimension.SQUARE_12),
					SortBy.COUNT, GuiHelper.getButtonStateData(new UV(36, 144), TranslationHelper.translButton("sort_by_count"), Dimension.SQUARE_12),
					SortBy.TAGS, GuiHelper.getButtonStateData(new UV(12, 144), TranslationHelper.translButton("sort_by_tags"), Dimension.SQUARE_12)
			));

	private static final TextureBlitData SORT_BUTTON_FOREGROUND = new TextureBlitData(ICONS, Dimension.SQUARE_256, new UV(0, 144), Dimension.SQUARE_12);
	public static final ButtonDefinition SORT = new ButtonDefinition(Dimension.SQUARE_12, SMALL_BUTTON_BACKGROUND, SMALL_BUTTON_HOVERED_BACKGROUND, SORT_BUTTON_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translButton("sort_action")));

	private static final TextureBlitData UPGRADE_SWITCH_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(65, 0), Dimension.RECTANGLE_6_12);
	private static final TextureBlitData UPGRADE_SWITCH_HOVERED_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(71, 0), Dimension.RECTANGLE_6_12);
	public static final ButtonDefinition.Toggle<Boolean> UPGRADE_SWITCH = new ButtonDefinition.Toggle<>(Dimension.RECTANGLE_6_12, UPGRADE_SWITCH_BACKGROUND, ImmutableMap.of(
			true, GuiHelper.getButtonStateData(new UV(4, 128), Dimension.RECTANGLE_4_10, new Position(1, 1), TranslationHelper.translColoredButton("upgrade_switch_enabled", TextFormatting.GREEN)),
			false, GuiHelper.getButtonStateData(new UV(0, 128), Dimension.RECTANGLE_4_10, new Position(1, 1), TranslationHelper.translColoredButton("upgrade_switch_disabled", TextFormatting.RED))
	), UPGRADE_SWITCH_HOVERED_BACKGROUND);

	public static final ButtonDefinition.Toggle<Boolean> WORK_IN_GUI = createToggleButtonDefinition(
			ImmutableMap.of(
					true, getButtonStateData(new UV(0, 48), translUpgradeButton("works_in_gui"), Dimension.SQUARE_16, new Position(1, 1)),
					false, getButtonStateData(new UV(16, 48), translUpgradeButton("only_automatic"), Dimension.SQUARE_16, new Position(1, 1))
			));

	public static final ButtonDefinition.Toggle<Boolean> SHIFT_CLICK_TARGET = createToggleButtonDefinition(
			ImmutableMap.of(
					true, getButtonStateData(new UV(32, 48), Dimension.SQUARE_16, new Position(1, 1),
							TranslationHelper.getTranslatedLines(translUpgradeButton("shift_click_into_backpack"), null)),
					false, getButtonStateData(new UV(48, 48), Dimension.SQUARE_16, new Position(1, 1),
							TranslationHelper.getTranslatedLines(translUpgradeButton("shift_click_into_inventory")))
			));
}
