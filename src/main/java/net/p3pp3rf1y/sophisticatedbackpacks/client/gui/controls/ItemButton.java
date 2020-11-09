package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;

import java.util.function.Predicate;

public class ItemButton extends ButtonBase {
	private static final int CORRECTION_OFFSET = -200; //because vanilla code renders item much higher than gui
	private final ItemStack stack;

	public ItemButton(int x, int y, Predicate<Integer> onClick, ItemStack stack) {
		super(x, y, 16, 16, onClick);
		this.stack = stack;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		GuiHelper.renderItemInGUI(minecraft, stack, x, y, zOffset + CORRECTION_OFFSET);
	}
}
