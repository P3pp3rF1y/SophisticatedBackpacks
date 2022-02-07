package net.p3pp3rf1y.sophisticatedcore.client.gui.controls;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.renderer.GameRenderer;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;

import java.util.function.Supplier;

public class ProgressBar extends WidgetBase {
	private final TextureBlitData progressTexture;
	private final Supplier<Float> getProgress;
	private final ProgressDirection dir;

	public ProgressBar(Position position, TextureBlitData progressTexture, Supplier<Float> getProgress, ProgressDirection dir) {
		super(position, new Dimension(progressTexture.getWidth(), progressTexture.getHeight()));
		this.progressTexture = progressTexture;
		this.getProgress = getProgress;
		this.dir = dir;
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		int height = progressTexture.getHeight();
		int width = progressTexture.getWidth();
		float progress = getProgress.get();
		if (progress <= 0) {
			return;
		}
		int yOffset = 0;
		if (dir == ProgressDirection.BOTTOM_UP) {
			yOffset = (int) (height * progress);
			height -= yOffset;
		} else if (dir == ProgressDirection.LEFT_RIGHT) {
			width = (int) (width * progress);
		}
		RenderSystem.setShader(GameRenderer::getPositionTexShader);
		RenderSystem.setShaderTexture(0, progressTexture.getTextureName());
		blit(matrixStack, x, y + yOffset, progressTexture.getU(), (float) progressTexture.getV() + yOffset, width, height, progressTexture.getTextureWidth(), progressTexture.getTextureHeight());
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}

	public enum ProgressDirection {
		LEFT_RIGHT,
		BOTTOM_UP
	}
}
