package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;

import java.util.function.IntConsumer;

public class ImageButton extends ButtonBase {
	private final TextureBlitData texture;

	public ImageButton(Position position, Dimension dimension, TextureBlitData texture, IntConsumer onClick) {
		super(position, dimension, onClick);
		this.texture = texture;
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		GuiHelper.blit(matrixStack, x, y, texture);
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}
}
