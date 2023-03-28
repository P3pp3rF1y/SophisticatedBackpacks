package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ImageButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsTab;

import java.util.List;
import java.util.Map;

import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper.getButtonStateData;

public class BackpackMainSettingsTab extends MainSettingsTab<BackpackMainSettingsContainer> {
	private static final TextureBlitData ICON = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(64, 48), Dimension.SQUARE_16);
	private static final List<Component> BACKPACK_CONTEXT_TOOLTIP = List.of(
			new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("context_backpack.tooltip")),
			new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("context_backpack.tooltip_detail")).withStyle(ChatFormatting.GRAY)
	);

	private static final ButtonDefinition.Toggle<Boolean> ANOTHER_PLAYER_CAN_OPEN = createToggleButtonDefinition(
			Map.of(
					true, getButtonStateData(new UV(176, 32), Dimension.SQUARE_16, new Position(1, 1),
							List.of(
									new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("another_player_can_open.on")),
									new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("another_player_can_open.on.tooltip")).withStyle(ChatFormatting.GRAY))
					),
					false, getButtonStateData(new UV(192, 32), Dimension.SQUARE_16, new Position(1, 1),
							List.of(
									new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("another_player_can_open.off")),
									new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("another_player_can_open.off.tooltip")).withStyle(ChatFormatting.GRAY))
					)
			));

	public BackpackMainSettingsTab(BackpackMainSettingsContainer container, Position position, SettingsScreen screen) {
		super(container, position, screen, BACKPACK_CONTEXT_TOOLTIP, new TranslatableComponent(SBPTranslationHelper.INSTANCE.translSettingsButton("context_backpack")),
				SBPTranslationHelper.INSTANCE.translSettings("backpack"), SBPTranslationHelper.INSTANCE.translSettingsTooltip("backpack"), onTabIconClicked -> new ImageButton(new Position(position.x() + 1, position.y() + 4), Dimension.SQUARE_16, ICON, onTabIconClicked));
		if (Boolean.TRUE.equals(Config.SERVER.allowOpeningOtherPlayerBackpacks.get())) {
			addHideableChild(new ToggleButton<>(new Position(x + 39, y + 46), ANOTHER_PLAYER_CAN_OPEN, button -> container.toggleAnotherPlayerCanOpen(), container::canAnotherPlayerOpen));
		}
	}
}
