package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable.GlobalOverridableSettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable.GlobalOverridableSettingsTab;

import java.util.List;

public class BackpackGlobalOverridableSettingsTab extends GlobalOverridableSettingsTab {
	private static final List<Component> BACKPACK_CONTEXT_TOOLTIP = List.of(
			new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("context_backpack.tooltip")),
			new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("context_backpack.tooltip_detail")).withStyle(ChatFormatting.GRAY)
	);

	public BackpackGlobalOverridableSettingsTab(GlobalOverridableSettingsContainer container, Position position, SettingsScreen screen) {
		super(container, position, screen, BACKPACK_CONTEXT_TOOLTIP, new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("context_backpack")),
				SBPTranslationHelper.INSTANCE.translSettings("backpack"), SBPTranslationHelper.INSTANCE.translSettingsTooltip("backpack"));
	}
}
