package net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ImageButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.SettingsTab;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.getButtonStateData;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.*;

public class BackpackSettingsTab extends SettingsTab<BackpackSettingsContainer> {
	private static final TextureBlitData ICON = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(64, 48), Dimension.SQUARE_16);
	private static final ButtonDefinition.Toggle<Boolean> SHIFT_CLICK_INTO_OPEN_TAB = createToggleButtonDefinition(
			ImmutableMap.of(
					true, getButtonStateData(new UV(80, 32), Dimension.SQUARE_16, new Position(1, 1),
							ImmutableList.of(
									new TranslationTextComponent(translSettingsButton("shift_click_open_tab.on")),
									new TranslationTextComponent(translSettingsButton("shift_click_open_tab.on.tooltip")).withStyle(TextFormatting.GRAY))
					),
					false, getButtonStateData(new UV(64, 96), Dimension.SQUARE_16, new Position(1, 1),
							ImmutableList.of(
									new TranslationTextComponent(translSettingsButton("shift_click_open_tab.off")),
									new TranslationTextComponent(translSettingsButton("shift_click_open_tab.off.tooltip")).withStyle(TextFormatting.GRAY))
					)
			));
	private static final ButtonDefinition.Toggle<Boolean> KEEP_TAB_OPEN = createToggleButtonDefinition(
			ImmutableMap.of(
					true, getButtonStateData(new UV(80, 80), Dimension.SQUARE_16, new Position(1, 1),
							ImmutableList.of(
									new TranslationTextComponent(translSettingsButton("keep_tab_open.on")),
									new TranslationTextComponent(translSettingsButton("keep_tab_open.on.tooltip")).withStyle(TextFormatting.GRAY))
					),
					false, getButtonStateData(new UV(80, 96), Dimension.SQUARE_16, new Position(1, 1),
							ImmutableList.of(
									new TranslationTextComponent(translSettingsButton("keep_tab_open.off")),
									new TranslationTextComponent(translSettingsButton("keep_tab_open.off.tooltip")).withStyle(TextFormatting.GRAY))
					)
			));
	private static final List<ITextComponent> PLAYER_CONTEXT_TOOLTIP = ImmutableList.of(
			new TranslationTextComponent(translSettingsButton("context_player.tooltip")),
			new TranslationTextComponent(translSettingsButton("context_player.tooltip_detail")).withStyle(TextFormatting.GRAY)
	);
	private static final List<ITextComponent> BACKPACK_CONTEXT_TOOLTIP = ImmutableList.of(
			new TranslationTextComponent(translSettingsButton("context_backpack.tooltip")),
			new TranslationTextComponent(translSettingsButton("context_backpack.tooltip_detail")).withStyle(TextFormatting.GRAY)
	);

	public BackpackSettingsTab(BackpackSettingsContainer container, Position position, SettingsScreen screen) {
		super(container, position, screen, new TranslationTextComponent(translSettings(BackpackSettingsCategory.NAME)),
				ImmutableList.of(new TranslationTextComponent(translSettingsTooltip(BackpackSettingsCategory.NAME))), Collections.emptyList(),
				onTabIconClicked -> new ImageButton(new Position(position.getX() + 1, position.getY() + 4), Dimension.SQUARE_16, ICON, onTabIconClicked));
		addHideableChild(new ContextButton(new Position(x + 3, y + 24), button -> container.toggleContext(),
				() -> new TranslationTextComponent(container.getContext() == Context.PLAYER ? translSettingsButton("context_player") : translSettingsButton("context_backpack")),
				() -> container.getContext() == Context.PLAYER ? PLAYER_CONTEXT_TOOLTIP : BACKPACK_CONTEXT_TOOLTIP));
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
