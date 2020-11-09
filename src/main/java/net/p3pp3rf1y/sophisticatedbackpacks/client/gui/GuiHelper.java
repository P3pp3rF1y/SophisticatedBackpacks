package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.AbstractGui;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.item.ItemStack;

public class GuiHelper {
	private GuiHelper() {}

	public static void renderItemInGUI(Minecraft minecraft, ItemStack stack, int xPosition, int yPosition, int zLevelOffset) {
		ItemRenderer itemRenderer = minecraft.getItemRenderer();
		float originalZLevel = itemRenderer.zLevel;
		itemRenderer.zLevel += zLevelOffset;
		itemRenderer.renderItemAndEffectIntoGUI(stack, xPosition, yPosition);
		itemRenderer.zLevel = originalZLevel;
	}

	public static void blit(Minecraft minecraft, MatrixStack matrixStack, int x, int y, TextureBlitData texData) {
		minecraft.getTextureManager().bindTexture(texData.getTextureName());
		AbstractGui.blit(matrixStack, x + texData.getXOffset(), y + texData.getYOffset(), texData.getU(), texData.getV(), texData.getWidth(), texData.getHeight(), texData.getTextureWidth(), texData.getTextureHeight());
	}
}
