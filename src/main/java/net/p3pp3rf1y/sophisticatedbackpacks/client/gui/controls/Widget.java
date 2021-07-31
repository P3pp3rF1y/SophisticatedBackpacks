package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.AbstractGui;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.IGuiEventListener;
import net.minecraft.client.gui.IRenderable;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

public abstract class Widget extends AbstractGui implements IRenderable, IGuiEventListener {
	protected final int x;

	protected final int y;
	protected int zOffset;
	protected final Minecraft minecraft;
	protected final FontRenderer font;

	protected Widget(Position position) {
		x = position.getX();
		y = position.getY();
		minecraft = Minecraft.getInstance();
		font = minecraft.font;
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (zOffset != 0) {
			matrixStack.pushPose();
			matrixStack.translate(0, 0, zOffset);
		}
		RenderSystem.enableDepthTest();
		renderBg(matrixStack, minecraft, mouseX, mouseY);
		renderWidget(matrixStack, mouseX, mouseY, partialTicks);
		if (zOffset != 0) {
			matrixStack.popPose();
		}
	}

	protected abstract void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY);

	protected abstract void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks);

	public abstract int getWidth();

	public abstract int getHeight();

	@Override
	public boolean isMouseOver(double mouseX, double mouseY) {
		return mouseX >= x && mouseX < x + getWidth() && mouseY >= y && mouseY < y + getHeight();
	}

	public void setZOffset(int zOffset) {
		this.zOffset = zOffset;
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

	protected int getCenteredX(int elementWidth) {
		return (getWidth() - elementWidth) / 2;
	}

	public void afterScreenRender(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		//noop
	}
}
