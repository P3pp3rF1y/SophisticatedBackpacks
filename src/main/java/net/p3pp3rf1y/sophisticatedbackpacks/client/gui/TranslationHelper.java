package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class TranslationHelper {
	private TranslationHelper() {}

	private static final String UPGRADE_PREFIX = "gui.sophisticatedbackpacks.upgrades.";
	private static final String UPGRADE_BUTTONS_PREFIX = UPGRADE_PREFIX + "buttons.";

	public static String translUpgrade(String upgradeName) {
		return UPGRADE_PREFIX + upgradeName;
	}

	public static String translUpgradeTooltip(String upgradeName) {
		return translUpgrade(upgradeName) + ".tooltip";
	}

	public static String translUpgradeButton(String buttonName) {
		return UPGRADE_BUTTONS_PREFIX + buttonName;
	}
}
