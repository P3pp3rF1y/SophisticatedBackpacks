package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.AbstractGui;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

public class BackpackGuiHelper {

	private BackpackGuiHelper() {}

	public static void renderBackpackBackground(Position position, MatrixStack matrixStack, int inventorySlots, int slotsOnLine, ResourceLocation textureName, int xSize, Minecraft minecraft, int numberOfSlotRows) {
		int x = position.getX();
		int y = position.getY();
		RenderSystem.color4f(1.0F, 1.0F, 1.0F, 1.0F);
		minecraft.getTextureManager().bind(textureName);
		int slotRows = inventorySlots / slotsOnLine;
		int remainingSlots = inventorySlots % slotsOnLine;
		int slotsHeight = 18 * numberOfSlotRows;
		int halfSlotHeight = slotsHeight / 2;
		AbstractGui.blit(matrixStack, x, y, 0, 0, xSize, BackpackScreen.SLOTS_Y_OFFSET + halfSlotHeight, 256, 256);
		int playerInventoryHeight = 97;
		AbstractGui.blit(matrixStack, x, y + BackpackScreen.SLOTS_Y_OFFSET + halfSlotHeight, 0, (float) 256 - (playerInventoryHeight + halfSlotHeight), xSize, playerInventoryHeight + halfSlotHeight, 256, 256);

		GuiHelper.renderSlotsBackground(minecraft, matrixStack, x + BackpackScreen.SLOTS_X_OFFSET, y + BackpackScreen.SLOTS_Y_OFFSET, slotsOnLine, slotRows, remainingSlots);
	}
}
