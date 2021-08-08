package net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import java.util.List;
import java.util.function.IntConsumer;
import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.GUI_CONTROLS;

public class ContextButton extends ButtonBase {
	public static final TextureBlitData LEFT_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(47, 0), new Dimension(16, 18));
	public static final TextureBlitData LEFT_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(29, 0), new Dimension(16, 18));
	public static final TextureBlitData MIDDLE_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(49, 0), new Dimension(14, 18));
	public static final TextureBlitData MIDDLE_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(31, 0), new Dimension(14, 18));
	public static final TextureBlitData RIGHT_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(49, 0), new Dimension(16, 18));
	public static final TextureBlitData RIGHT_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(31, 0), new Dimension(16, 18));
	private final Supplier<ITextComponent> getTitle;
	private final Supplier<List<ITextComponent>> getTooltipKey;

	protected ContextButton(Position position, IntConsumer onClick, Supplier<ITextComponent> getTitle, Supplier<List<ITextComponent>> getTooltipKey) {
		super(position, new Dimension(62, 18), onClick);
		this.getTitle = getTitle;
		this.getTooltipKey = getTooltipKey;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		if (isMouseOver(mouseX, mouseY)) {
			renderBackground(matrixStack, minecraft, LEFT_BUTTON_HOVERED_BACKGROUND, MIDDLE_BUTTON_HOVERED_BACKGROUND, RIGHT_BUTTON_HOVERED_BACKGROUND);
			GuiHelper.setTooltipToRender(getTooltipKey.get());
		} else {
			renderBackground(matrixStack, minecraft, LEFT_BUTTON_BACKGROUND, MIDDLE_BUTTON_BACKGROUND, RIGHT_BUTTON_BACKGROUND);
		}
	}

	private void renderBackground(MatrixStack matrixStack, Minecraft minecraft, TextureBlitData leftButtonHoveredBackground, TextureBlitData middleButtonHoveredBackground, TextureBlitData rightButtonHoveredBackground) {
		int left = x;
		GuiHelper.blit(minecraft, matrixStack, left, y, leftButtonHoveredBackground);
		left += leftButtonHoveredBackground.getWidth();
		GuiHelper.blit(minecraft, matrixStack, left, y, middleButtonHoveredBackground);
		left += middleButtonHoveredBackground.getWidth();
		GuiHelper.blit(minecraft, matrixStack, left, y, middleButtonHoveredBackground);
		left += middleButtonHoveredBackground.getWidth();
		GuiHelper.blit(minecraft, matrixStack, left, y, rightButtonHoveredBackground);
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		drawCenteredString(matrixStack, Minecraft.getInstance().font, getTitle.get(), x + width / 2, y - 4 + height / 2, 16777215 | (255 << 24));
	}
}
