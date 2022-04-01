package net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.inventory.Slot;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsTab;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.IntConsumer;

import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper.getButtonStateData;

public class GlobalOverridableSettingsTab extends SettingsTab<GlobalOverridableSettingsContainer> {
	private static final ButtonDefinition.Toggle<Boolean> SHIFT_CLICK_INTO_OPEN_TAB = createToggleButtonDefinition(
			Map.of(
					true, getButtonStateData(new UV(80, 32), Dimension.SQUARE_16, new Position(1, 1),
							List.of(
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("shift_click_open_tab.on")),
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("shift_click_open_tab.on.tooltip")).withStyle(ChatFormatting.GRAY))
					),
					false, getButtonStateData(new UV(64, 96), Dimension.SQUARE_16, new Position(1, 1),
							List.of(
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("shift_click_open_tab.off")),
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("shift_click_open_tab.off.tooltip")).withStyle(ChatFormatting.GRAY))
					)
			));
	private static final ButtonDefinition.Toggle<Boolean> KEEP_TAB_OPEN = createToggleButtonDefinition(
			Map.of(
					true, getButtonStateData(new UV(80, 80), Dimension.SQUARE_16, new Position(1, 1),
							List.of(
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("keep_tab_open.on")),
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("keep_tab_open.on.tooltip")).withStyle(ChatFormatting.GRAY))
					),
					false, getButtonStateData(new UV(80, 96), Dimension.SQUARE_16, new Position(1, 1),
							List.of(
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("keep_tab_open.off")),
									new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("keep_tab_open.off.tooltip")).withStyle(ChatFormatting.GRAY))
					)
			));
	private static final List<Component> PLAYER_CONTEXT_TOOLTIP = List.of(
			new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("context_player.tooltip")),
			new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("context_player.tooltip_detail")).withStyle(ChatFormatting.GRAY)
	);

	public GlobalOverridableSettingsTab(GlobalOverridableSettingsContainer container, Position position, SettingsScreen screen, List<Component> storageContextTooltip, Component storageContextTitle, String tabLabelTranslKey, String tabTooltipTranslKey, Function<IntConsumer, ButtonBase> getTabButton) {
		super(container, position, screen, new TranslatableComponent(tabLabelTranslKey),
				List.of(new TranslatableComponent(tabTooltipTranslKey)), Collections.emptyList(),
				getTabButton);
		addHideableChild(new ContextButton(new Position(x + 3, y + 24), button -> container.toggleContext(),
				() -> container.getContext() == Context.PLAYER ? new TranslatableComponent(TranslationHelper.INSTANCE.translSettingsButton("context_player")) : storageContextTitle,
				() -> container.getContext() == Context.PLAYER ? PLAYER_CONTEXT_TOOLTIP : storageContextTooltip));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 46), SHIFT_CLICK_INTO_OPEN_TAB, button -> container.toggleShiftClickIntoOpenTab(), container::shouldShiftClickIntoOpenTab));
		addHideableChild(new ToggleButton<>(new Position(x + 21, y + 46), KEEP_TAB_OPEN, button -> container.toggleKeepTabOpen(), container::shouldKeepTabOpen));
	}

	@Override
	public Optional<Integer> getSlotOverlayColor(int slotNumber) {
		return Optional.empty();
	}

	@Override
	public void handleSlotClick(Slot slot, int mouseButton) {
		//noop
	}
}
