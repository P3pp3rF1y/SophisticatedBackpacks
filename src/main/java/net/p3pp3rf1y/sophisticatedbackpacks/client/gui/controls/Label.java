package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;

public class Label extends Widget {
	public static final int DEFAULT_GUI_TEXT_COLOR = 4210752;
	private final String labelText;
	private final int color;

	public Label(int x, int y, String labelText) {
		this(x, y, labelText, DEFAULT_GUI_TEXT_COLOR);
	}

	public Label(int x, int y, String labelText, int color) {
		super(x, y);
		this.labelText = labelText;
		this.color = color;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		minecraft.fontRenderer.drawString(matrixStack, labelText, x, y, color);
	}

	@Override
	public int getWidth() {
		return 0;
	}

	@Override
	public int getHeight() {
		return 0;
	}
}
