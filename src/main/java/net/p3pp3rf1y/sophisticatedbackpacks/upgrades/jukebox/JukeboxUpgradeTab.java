package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Button;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

public class JukeboxUpgradeTab extends UpgradeSettingsTab<JukeboxUpgradeContainer> {
	private static final TextureBlitData PLAY_FOREGROUND = new TextureBlitData(GUI_CONTROLS, new Position(1, 1), Dimension.SQUARE_256, new UV(48, 96), Dimension.SQUARE_16);
	private static final ButtonDefinition PLAY = new ButtonDefinition(Dimension.SQUARE_16, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, PLAY_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translUpgradeButton("play")));
	private static final TextureBlitData STOP_FOREGROUND = new TextureBlitData(GUI_CONTROLS, new Position(1, 1), Dimension.SQUARE_256, new UV(32, 96), Dimension.SQUARE_16);
	private static final ButtonDefinition STOP = new ButtonDefinition(Dimension.SQUARE_16, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, STOP_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translUpgradeButton("stop")));

	public JukeboxUpgradeTab(JukeboxUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("jukebox")),
				new TranslationTextComponent(translUpgradeTooltip("jukebox")));

		addHideableChild(new Button(new Position(x + 3, y + 44), STOP, button -> {
			if (button == 0) {
				getContainer().stop();
			}
		}));
		addHideableChild(new Button(new Position(x + 21, y + 44), PLAY, button -> {
			if (button == 0) {
				getContainer().play();
			}
		}));
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (getContainer().isOpen()) {
			GuiHelper.renderSlotsBackground(minecraft, matrixStack, x + 3, y + 24, 1, 1);
		}
	}

	@Override
	protected void moveSlotsToTab() {
		Slot discSlot = getContainer().getSlots().get(0);
		discSlot.xPos = x - screen.getGuiLeft() + 4;
		discSlot.yPos = y - screen.getGuiTop() + 25;
	}
}
