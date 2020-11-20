package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import javax.annotation.Nullable;
import java.util.function.Predicate;

@OnlyIn(Dist.CLIENT)
public class Button extends ButtonBase {
	private final TextureBlitData backgroundTexture;
	private TextureBlitData hoveredBackgroundTexture = null;
	private TextureBlitData foregroundTexture = null;

	public Button(Position position, Dimension dimension, Predicate<Integer> onClick, TextureBlitData backgroundTexture) {
		this(position, dimension, onClick, backgroundTexture, null);
	}

	public void setHoveredBackgroundTexture(TextureBlitData hoveredBackgroundTexture) {
		this.hoveredBackgroundTexture = hoveredBackgroundTexture;
	}

	public void setForegroundTexture(TextureBlitData foregroundTexture) {
		this.foregroundTexture = foregroundTexture;
	}

	public Button(Position position, Dimension dimension, Predicate<Integer> onClick, TextureBlitData backgroundTexture,
			@Nullable TextureBlitData foregroundTexture) {
		super(position, dimension, onClick);
		this.backgroundTexture = backgroundTexture;
		this.foregroundTexture = foregroundTexture;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		if (isMouseOver(mouseX, mouseY)) {
			GuiHelper.blit(minecraft, matrixStack, x, y, hoveredBackgroundTexture);
		} else {
			GuiHelper.blit(minecraft, matrixStack, x, y, backgroundTexture);
		}
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (foregroundTexture != null) {
			GuiHelper.blit(minecraft, matrixStack, x, y, foregroundTexture);
		}
	}
}
