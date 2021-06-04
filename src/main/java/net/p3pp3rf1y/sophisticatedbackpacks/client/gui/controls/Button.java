package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.util.text.ITextProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;
import java.util.function.IntConsumer;

public class Button extends ButtonBase {
	private final TextureBlitData backgroundTexture;
	@Nullable
	private final TextureBlitData hoveredBackgroundTexture;
	@Nullable
	private final TextureBlitData foregroundTexture;
	private final List<ITextProperties> tooltip;

	public Button(Position position, ButtonDefinition buttonDefinition, IntConsumer onClick) {
		super(position, buttonDefinition.getDimension(), onClick);
		backgroundTexture = buttonDefinition.getBackgroundTexture();
		foregroundTexture = buttonDefinition.getForegroundTexture();
		hoveredBackgroundTexture = buttonDefinition.getHoveredBackgroundTexture();
		tooltip = Collections.singletonList(buttonDefinition.getTooltip());
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		if (isMouseOver(mouseX, mouseY)) {
			if (hoveredBackgroundTexture != null) {
				GuiHelper.blit(minecraft, matrixStack, x, y, hoveredBackgroundTexture);
			}
		} else {
			GuiHelper.blit(minecraft, matrixStack, x, y, backgroundTexture);
		}
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (foregroundTexture != null) {
			GuiHelper.blit(minecraft, matrixStack, x, y, foregroundTexture);
		}
		if (isMouseOver(mouseX, mouseY)) {
			GuiHelper.setTooltipToRender(tooltip);
		}
	}
}
