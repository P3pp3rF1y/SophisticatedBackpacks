package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils;

import net.minecraft.client.resources.I18n;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

public class TranslationHelper {

	private static final String TOOLTIP_SUFFIX = ".tooltip";

	private TranslationHelper() {}

	private static final String GUI_PREFIX = "gui.sophisticatedbackpacks.";

	private static final String GUI_UPGRADE_PREFIX = GUI_PREFIX + "upgrades.";
	private static final String GUI_SETTINGS_PREFIX = GUI_PREFIX + "settings.";
	private static final String BUTTONS_SUFFIX = "buttons.";
	private static final String BUTTONS_PREFIX = GUI_PREFIX + BUTTONS_SUFFIX;
	private static final String ITEM_UPGRADE_PREFIX = "item.sophisticatedbackpacks.";
	private static final String UPGRADE_BUTTONS_PREFIX = GUI_UPGRADE_PREFIX + BUTTONS_SUFFIX;
	private static final String KEYBIND_PREFIX = "keybind.sophisticatedbackpacks.";

	public static ITextComponent translUpgrade(String upgradeName) {
		return new TranslationTextComponent(translUpgradeKey(upgradeName));
	}

	public static ITextComponent translUpgradeSlotTooltip(String tooltipName) {
		return new TranslationTextComponent(GUI_UPGRADE_PREFIX + "slots." + tooltipName + TOOLTIP_SUFFIX);
	}

	public static String translUpgradeKey(String upgradeName) {
		return GUI_UPGRADE_PREFIX + upgradeName;
	}

	public static String translSettings(String categoryName) {
		return GUI_SETTINGS_PREFIX + categoryName;
	}

	public static String translSettingsButton(String buttonName) {
		return translSettings(BUTTONS_SUFFIX + buttonName);
	}

	public static ITextComponent translUpgradeTooltip(String upgradeName) {
		return new TranslationTextComponent(translUpgradeKey(upgradeName) + TOOLTIP_SUFFIX);
	}

	public static String translSettingsTooltip(String categoryName) {
		return translSettings(categoryName) + TOOLTIP_SUFFIX;
	}

	public static ITextComponent translColoredButton(String buttonName, TextFormatting color) {
		return new TranslationTextComponent(translButton(buttonName)).withStyle(color);
	}

	public static String translButton(String buttonName) {
		return BUTTONS_PREFIX + buttonName;
	}

	public static ITextComponent translError(String key, Object... params) {
		return new TranslationTextComponent(GUI_PREFIX + "error." + key, params);
	}

	public static String translUpgradeButton(String buttonName) {
		return UPGRADE_BUTTONS_PREFIX + buttonName;
	}

	public static String translItemTooltip(String itemName) {
		return ITEM_UPGRADE_PREFIX + itemName + TOOLTIP_SUFFIX;
	}

	public static List<StringTextComponent> getTranslatedLines(String translateKey, @Nullable Object parameters, TextFormatting... textFormattings) {
		List<StringTextComponent> ret = getTranslatedLines(translateKey, parameters);
		ret.forEach(l -> l.withStyle(textFormattings));
		return ret;
	}

	public static List<StringTextComponent> getTranslatedLines(String translateKey) {
		return getTranslatedLines(translateKey, null);
	}

	public static List<StringTextComponent> getTranslatedLines(String translateKey, @Nullable Object parameters) {
		String text = translate(translateKey, parameters);

		String[] lines = text.split("\n");

		List<StringTextComponent> ret = new ArrayList<>();
		for (String line : lines) {
			ret.add(new StringTextComponent(line));
		}

		return ret;
	}

	public static String translate(String translateKey, Object... parameters) {
		return I18n.get(translateKey, parameters);
	}

	public static String translKeybind(String keybindName) {
		return KEYBIND_PREFIX + keybindName;
	}

	public static String translGui(String guiTranslateKey) {
		return GUI_PREFIX + guiTranslateKey;
	}
}
