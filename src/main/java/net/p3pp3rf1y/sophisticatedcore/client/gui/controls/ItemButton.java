package net.p3pp3rf1y.sophisticatedcore.client.gui.controls;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarratedElementType;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;

import java.util.function.IntConsumer;

public class ItemButton extends ButtonBase {
	private final ItemStack stack;
	private final Component narration;

	public ItemButton(Position position, IntConsumer onClick, ItemStack stack, Component narration) {
		super(position, Dimension.SQUARE_16, onClick);
		this.stack = stack;
		this.narration = narration;
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		GuiHelper.renderItemInGUI(matrixStack, minecraft, stack, x, y);
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		pNarrationElementOutput.add(NarratedElementType.TITLE, narration);
		pNarrationElementOutput.add(NarratedElementType.USAGE, new TranslatableComponent("narration.button.usage.focused"));
	}
}
