package net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort;

import com.google.common.collect.ImmutableList;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Button;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ImageButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ColorToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.SettingsTab;

import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_HOVERED_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translSettings;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translSettingsTooltip;

public class NoSortSettingsTab extends SettingsTab<NoSortSettingsContainer> {
	private static final TextureBlitData ICON = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(32, 80), Dimension.SQUARE_16);
	private static final TextureBlitData SELECT_ALL_SLOTS_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(16, 80), Dimension.SQUARE_16);
	public static final ButtonDefinition SELECT_ALL_SLOTS = new ButtonDefinition(Dimension.SQUARE_16, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, SELECT_ALL_SLOTS_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translSettingsButton("select_all_slots")));
	private static final TextureBlitData UNSELECT_ALL_SLOTS_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(48, 80), Dimension.SQUARE_16);
	public static final ButtonDefinition UNSELECT_ALL_SLOTS = new ButtonDefinition(Dimension.SQUARE_16, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, UNSELECT_ALL_SLOTS_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translSettingsButton("unselect_all_slots")));

	public NoSortSettingsTab(NoSortSettingsContainer container, Position position, SettingsScreen screen) {
		super(container, position, screen, new TranslationTextComponent(translSettings(NoSortSettingsCategory.NAME)),
				new ImmutableList.Builder<ITextProperties>()
						.add(new TranslationTextComponent(translSettingsTooltip(NoSortSettingsCategory.NAME)))
						.addAll(TranslationHelper.getTranslatedLines(translSettingsTooltip(NoSortSettingsCategory.NAME) + "_detail", null, TextFormatting.GRAY))
						.build(),
				new ImmutableList.Builder<ITextProperties>()
						.add(new TranslationTextComponent(translSettingsTooltip(NoSortSettingsCategory.NAME)))
						.addAll(TranslationHelper.getTranslatedLines(translSettingsTooltip(NoSortSettingsCategory.NAME) + "_open_detail", null, TextFormatting.GRAY))
						.build(),
				onTabIconClicked -> new ImageButton(new Position(position.getX() + 1, position.getY() + 4), Dimension.SQUARE_16, ICON, onTabIconClicked));
		addHideableChild(new Button(new Position(x + 3, y + 24), SELECT_ALL_SLOTS, button -> container.selectAllSlots()));
		addHideableChild(new Button(new Position(x + 21, y + 24), UNSELECT_ALL_SLOTS, button -> container.unselectAllSlots()));
		addHideableChild(new ColorToggleButton(new Position(x + 39, y + 24), container::getColor, container::setColor));
	}

	@Override
	public Optional<Integer> getSlotOverlayColor(int slotNumber) {
		return getSettingsContainer().isSlotSelected(slotNumber) ? Optional.of(getSettingsContainer().getColor().getColorValue() | (80 << 24)) : Optional.empty();
	}

	@Override
	public void handleSlotClick(Slot slot, int mouseButton) {
		if (mouseButton == 0) {
			getSettingsContainer().selectSlot(slot.index);
		} else if (mouseButton == 1) {
			getSettingsContainer().unselectSlot(slot.index);
		}
	}
}
