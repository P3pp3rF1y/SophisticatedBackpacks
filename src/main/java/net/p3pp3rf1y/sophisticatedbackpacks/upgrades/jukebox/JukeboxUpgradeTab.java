package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Button;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public class JukeboxUpgradeTab extends UpgradeSettingsTab<JukeboxUpgradeContainer> {
	private static final TextureBlitData PLAY_FOREGROUND = new TextureBlitData(ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(16, 64), Dimension.SQUARE_16);
	private static final ButtonDefinition PLAY = new ButtonDefinition(Dimension.SQUARE_16, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, PLAY_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translUpgradeButton("play")));
	private static final TextureBlitData STOP_FOREGROUND = new TextureBlitData(ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(0, 64), Dimension.SQUARE_16);
	private static final ButtonDefinition STOP = new ButtonDefinition(Dimension.SQUARE_16, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, STOP_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translUpgradeButton("stop")));

	public JukeboxUpgradeTab(JukeboxUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, translUpgrade("jukebox"), translUpgradeTooltip("jukebox"));

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
		discSlot.x = x - screen.getGuiLeft() + 4;
		discSlot.y = y - screen.getGuiTop() + 25;
	}
}
