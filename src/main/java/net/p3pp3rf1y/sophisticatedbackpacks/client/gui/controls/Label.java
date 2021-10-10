package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

public class Label extends BackpackWidget {
	private static final int DEFAULT_GUI_TEXT_COLOR = 4210752;
	private final Component labelText;
	private final int color;

	public Label(Position position, Component labelText) {
		this(position, labelText, DEFAULT_GUI_TEXT_COLOR);
	}

	public Label(Position position, Component labelText, int color) {
		super(position, new Dimension(Minecraft.getInstance().font.width(labelText), 8));
		this.labelText = labelText;
		this.color = color;
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		minecraft.font.draw(matrixStack, labelText, x, y, color);
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}
}
