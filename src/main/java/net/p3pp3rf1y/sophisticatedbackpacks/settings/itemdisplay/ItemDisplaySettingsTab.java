package net.p3pp3rf1y.sophisticatedbackpacks.settings.itemdisplay;

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

import java.util.List;
import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_HOVERED_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translSettings;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translSettingsTooltip;

public class ItemDisplaySettingsTab extends SettingsTab<ItemDisplaySettingsContainer> {
	private static final TextureBlitData ICON = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(112, 64), Dimension.SQUARE_16);
	private static final List<ITextProperties> ROTATE_TOOLTIP = new ImmutableList.Builder<ITextProperties>()
			.add(new TranslationTextComponent(TranslationHelper.translSettingsButton("rotate")))
			.addAll(TranslationHelper.getTranslatedLines(TranslationHelper.translSettingsButton("rotate_detail"), null, TextFormatting.GRAY))
			.build();
	private static final TextureBlitData ROTATE_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(128, 64), Dimension.SQUARE_16);
	public static final ButtonDefinition ROTATE = new ButtonDefinition(Dimension.SQUARE_16, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, ROTATE_FOREGROUND);

	public ItemDisplaySettingsTab(ItemDisplaySettingsContainer container, Position position, SettingsScreen screen) {
		super(container, position, screen, new TranslationTextComponent(translSettings(ItemDisplaySettingsCategory.NAME)),
				new ImmutableList.Builder<ITextProperties>()
						.add(new TranslationTextComponent(translSettingsTooltip(ItemDisplaySettingsCategory.NAME)))
						.addAll(TranslationHelper.getTranslatedLines(translSettingsTooltip(ItemDisplaySettingsCategory.NAME) + "_detail", null, TextFormatting.GRAY))
						.build(),
				new ImmutableList.Builder<ITextProperties>()
						.add(new TranslationTextComponent(translSettingsTooltip(ItemDisplaySettingsCategory.NAME)))
						.addAll(TranslationHelper.getTranslatedLines(translSettingsTooltip(ItemDisplaySettingsCategory.NAME) + "_open_detail", null, TextFormatting.GRAY))
						.build(),
				onTabIconClicked -> new ImageButton(new Position(position.getX() + 1, position.getY() + 4), Dimension.SQUARE_16, ICON, onTabIconClicked));
		addHideableChild(new Button(new Position(x + 3, y + 24), ROTATE, button -> {
			if (button == 0) {
				container.rotateClockwise();
			} else if (button == 1) {
				container.rotateCounterClockwise();
			}
		}) {
			@Override
			protected List<ITextProperties> getTooltip() {
				return ROTATE_TOOLTIP;
			}
		});
		addHideableChild(new ColorToggleButton(new Position(x + 21, y + 24), container::getColor, container::setColor));
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

	@Override
	public int getItemRotation(int slotIndex) {
		return getSettingsContainer().getSlot().orElse(-1) == slotIndex ? getSettingsContainer().getRotation() : 0;
	}
}
