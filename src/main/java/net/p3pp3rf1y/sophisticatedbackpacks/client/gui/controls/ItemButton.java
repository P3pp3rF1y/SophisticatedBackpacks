package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;

import java.util.function.Consumer;

public class ItemButton extends ButtonBase {
	private final ItemStack stack;

	public ItemButton(Position position, Consumer<Integer> onClick, ItemStack stack) {
		super(position, new Dimension(16, 16), onClick);
		this.stack = stack;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		GuiHelper.renderItemInGUI(matrixStack, minecraft, stack, x, y);
	}
}
