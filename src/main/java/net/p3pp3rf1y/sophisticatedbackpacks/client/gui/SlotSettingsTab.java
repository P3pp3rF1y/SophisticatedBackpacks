package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ImageButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

public class SlotSettingsTab extends Tab {
	private static final TextureBlitData ICON = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(112, 0), Dimension.SQUARE_16);
	private final BackpackScreen screen;

	public SlotSettingsTab(Position position, BackpackScreen screen) {
		super(position, new TranslationTextComponent(TranslationHelper.translGui("slot_settings.tooltip")), onTabIconClicked -> new ImageButton(new Position(position.getX() + 1, position.getY() + 4), Dimension.SQUARE_16, ICON, onTabIconClicked));
		this.screen = screen;
	}

	@Override
	protected void onTabIconClicked(int button) {
		screen.getContainer().openSlotSettings();
	}
}
