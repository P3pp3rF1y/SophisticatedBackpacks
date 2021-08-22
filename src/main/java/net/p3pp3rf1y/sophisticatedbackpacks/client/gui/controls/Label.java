package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

public class Label extends Widget {
	public static final int DEFAULT_GUI_TEXT_COLOR = 4210752;
	private final ITextComponent labelText;
	private final int color;

	public Label(Position position, ITextComponent labelText) {
		this(position, labelText, DEFAULT_GUI_TEXT_COLOR);
	}

	public Label(Position position, ITextComponent labelText, int color) {
		super(position);
		this.labelText = labelText;
		this.color = color;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		minecraft.font.draw(matrixStack, labelText, x, y, color);
	}

	@Override
	public int getWidth() {
		return minecraft.font.width(labelText);
	}

	@Override
	public int getHeight() {
		return 8;
	}
}
