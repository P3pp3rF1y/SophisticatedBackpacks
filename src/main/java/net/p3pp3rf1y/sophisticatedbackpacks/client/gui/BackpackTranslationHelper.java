package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;

public class BackpackTranslationHelper extends TranslationHelper {
	public static final BackpackTranslationHelper INSTANCE = new BackpackTranslationHelper();

	private BackpackTranslationHelper() {
		super(SophisticatedBackpacks.MOD_ID);
	}
}
