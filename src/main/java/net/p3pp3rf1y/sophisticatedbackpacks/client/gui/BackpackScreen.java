package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.gui.screen.inventory.ContainerScreen;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import java.util.HashMap;
import java.util.Map;

public class BackpackScreen extends ContainerScreen<BackpackContainer> {
	private static final Map<Integer, ResourceLocation> BACKPACK_TEXTURES = new HashMap<>();

	public BackpackScreen(BackpackContainer screenContainer, PlayerInventory inv, ITextComponent titleIn) {
		super(screenContainer, inv, titleIn);
		ySize = 114 + getContainer().getNumberOfRows() * 18;
		xSize = getContainer().getScreenProperties().getSlotsOnLine() * 18 + 14;
		playerInventoryTitleY = ySize - 94;
		playerInventoryTitleX = 8 + getContainer().getScreenProperties().getPlayerInventoryYOffset();
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		renderBackground(matrixStack);
		super.render(matrixStack, mouseX, mouseY, partialTicks);
		renderHoveredTooltip(matrixStack, mouseX, mouseY);
	}

	protected void drawGuiContainerBackgroundLayer(MatrixStack matrixStack, float partialTicks, int x, int y) {
		RenderSystem.color4f(1.0F, 1.0F, 1.0F, 1.0F);
		minecraft.getTextureManager().bindTexture(getBackpackTexture(container.getNumberOfSlots()));
		int i = (width - xSize) / 2;
		int j = (height - ySize) / 2;
		int textureSize = container.getScreenProperties().getTextureSize();
		blit(matrixStack, i, j, 0, 0, xSize, ySize, textureSize, textureSize);
	}

	private static ResourceLocation getBackpackTexture(int numberOfSlots) {
		if (!BACKPACK_TEXTURES.containsKey(numberOfSlots)) {
			BACKPACK_TEXTURES.put(numberOfSlots, new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/gui/backpack_" + numberOfSlots + ".png"));
		}

		return BACKPACK_TEXTURES.get(numberOfSlots);
	}
}
