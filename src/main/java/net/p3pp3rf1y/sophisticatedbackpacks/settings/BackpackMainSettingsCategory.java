package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.settings.MainSetting;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsManager;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.function.Consumer;

public class BackpackMainSettingsCategory extends MainSettingsCategory {
	public static final MainSetting<Boolean> ANOTHER_PLAYER_CAN_OPEN =
			new MainSetting<>("anotherPlayerCanOpen", NBTHelper::getBoolean, CompoundTag::putBoolean, true);

	public static final String NAME = "backpackGlobal";

	static {
		SettingsManager.addSetting(ANOTHER_PLAYER_CAN_OPEN);
	}

	public BackpackMainSettingsCategory(CompoundTag categoryNbt, Consumer<CompoundTag> saveNbt, String playerSettingsTagName) {
		super(categoryNbt, saveNbt, playerSettingsTagName);
	}
}
