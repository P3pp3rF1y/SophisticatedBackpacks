package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.AbstractGui;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.WorldVertexBufferUploader;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.item.ItemStack;
import net.minecraft.util.IReorderingProcessor;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.vector.Matrix4f;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GuiHelper {
	public static final ResourceLocation GUI_CONTROLS = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/gui/gui_controls.png");
	public static final TextureBlitData DEFAULT_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(47, 0), Dimension.SQUARE_18);
	public static final TextureBlitData DEFAULT_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(29, 0), Dimension.SQUARE_18);
	public static final TextureBlitData SMALL_BUTTON_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 18), Dimension.SQUARE_12);
	public static final TextureBlitData SMALL_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(41, 18), Dimension.SQUARE_12);
	public static final ResourceLocation SLOTS_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/gui/slots_background.png");

	private static final Map<Integer, TextureBlitData> SLOTS_BACKGROUNDS = new HashMap<>();

	private GuiHelper() {}

	public static void renderItemInGUI(MatrixStack matrixStack, Minecraft minecraft, ItemStack stack, int xPosition, int yPosition) {
		renderItemInGUI(matrixStack, minecraft, stack, xPosition, yPosition, false);
	}

	public static void renderSlotsBackground(Minecraft minecraft, MatrixStack matrixStack, int x, int y, int slotWidth, int slotHeight) {
		int key = getSlotsBackgroundKey(slotWidth, slotHeight);
		blit(minecraft, matrixStack, x, y, SLOTS_BACKGROUNDS.computeIfAbsent(key, k ->
				new TextureBlitData(SLOTS_BACKGROUND, Dimension.SQUARE_256, new UV(0, 0), new Dimension(slotWidth * 18, slotHeight * 18))
		));
	}

	private static int getSlotsBackgroundKey(int slotWidth, int slotHeight) {
		return slotWidth * 31 + slotHeight;
	}

	public static void renderItemInGUI(MatrixStack matrixStack, Minecraft minecraft, ItemStack stack, int xPosition, int yPosition, boolean renderOverlay) {
		ItemRenderer itemRenderer = minecraft.getItemRenderer();
		float originalZLevel = itemRenderer.zLevel;
		itemRenderer.zLevel += getZOffset(matrixStack);
		itemRenderer.renderItemAndEffectIntoGUI(stack, xPosition, yPosition);
		if (renderOverlay) {
			itemRenderer.renderItemOverlayIntoGUI(minecraft.fontRenderer, stack, xPosition, yPosition, null);
		}
		itemRenderer.zLevel = originalZLevel;
	}

	private static int getZOffset(MatrixStack matrixStack) {
		Float zOffset = ObfuscationReflectionHelper.getPrivateValue(Matrix4f.class, matrixStack.getLast().getMatrix(), "field_226586_l_");
		return zOffset == null ? 0 : zOffset.intValue();
	}

	public static void blit(Minecraft minecraft, MatrixStack matrixStack, int x, int y, TextureBlitData texData) {
		minecraft.getTextureManager().bindTexture(texData.getTextureName());
		AbstractGui.blit(matrixStack, x + texData.getXOffset(), y + texData.getYOffset(), texData.getU(), texData.getV(), texData.getWidth(), texData.getHeight(), texData.getTextureWidth(), texData.getTextureHeight());
	}

	private static List<? extends IReorderingProcessor> tooltipToRender = Collections.emptyList();
	@Nullable
	private static FontRenderer tooltipRenderFont = null;

	public static void setTooltipToRender(List<IReorderingProcessor> tooltip) {
		setTooltipToRender(tooltip, null);
	}

	public static void setTooltipToRender(List<IReorderingProcessor> tooltip, @Nullable FontRenderer font) {
		tooltipToRender = tooltip;
		tooltipRenderFont = font;
	}

	public static void renderToolTip(Minecraft minecraft, MatrixStack matrixStack, int mouseX, int mouseY) {
		if (tooltipToRender.isEmpty()) {
			return;
		}

		FontRenderer font = tooltipRenderFont == null ? minecraft.fontRenderer : tooltipRenderFont;
		int windowWidth = minecraft.getMainWindow().getScaledWidth();
		int windowHeight = minecraft.getMainWindow().getScaledHeight();

		int maxLineWidth = getMaxLineWidth(tooltipToRender, font);

		int leftX = mouseX + 12;
		int topY = mouseY - 12;
		int tooltipHeight = 8;
		if (tooltipToRender.size() > 1) {
			tooltipHeight += 2 + (tooltipToRender.size() - 1) * 10;
		}

		if (leftX + maxLineWidth > windowWidth) {
			leftX -= 28 + maxLineWidth;
		}

		if (topY + tooltipHeight + 6 > windowHeight) {
			topY = windowHeight - tooltipHeight - 6;
		}

		matrixStack.push();
		Tessellator tessellator = Tessellator.getInstance();
		BufferBuilder bufferbuilder = tessellator.getBuffer();
		bufferbuilder.begin(7, DefaultVertexFormats.POSITION_COLOR);
		Matrix4f matrix4f = matrixStack.getLast().getMatrix();
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 4, leftX + maxLineWidth + 3, topY - 3, 400, -267386864, -267386864);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY + tooltipHeight + 3, leftX + maxLineWidth + 3, topY + tooltipHeight + 4, 400, -267386864, -267386864);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 3, leftX + maxLineWidth + 3, topY + tooltipHeight + 3, 400, -267386864, -267386864);
		fillGradient(matrix4f, bufferbuilder, leftX - 4, topY - 3, leftX - 3, topY + tooltipHeight + 3, 400, -267386864, -267386864);
		fillGradient(matrix4f, bufferbuilder, leftX + maxLineWidth + 3, topY - 3, leftX + maxLineWidth + 4, topY + tooltipHeight + 3, 400, -267386864, -267386864);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 3 + 1, leftX - 3 + 1, topY + tooltipHeight + 3 - 1, 400, 1347420415, 1344798847);
		fillGradient(matrix4f, bufferbuilder, leftX + maxLineWidth + 2, topY - 3 + 1, leftX + maxLineWidth + 3, topY + tooltipHeight + 3 - 1, 400, 1347420415, 1344798847);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 3, leftX + maxLineWidth + 3, topY - 3 + 1, 400, 1347420415, 1347420415);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY + tooltipHeight + 2, leftX + maxLineWidth + 3, topY + tooltipHeight + 3, 400, 1344798847, 1344798847);
		RenderSystem.enableDepthTest();
		RenderSystem.disableTexture();
		RenderSystem.enableBlend();
		RenderSystem.defaultBlendFunc();
		RenderSystem.shadeModel(7425);
		bufferbuilder.finishDrawing();
		WorldVertexBufferUploader.draw(bufferbuilder);
		RenderSystem.shadeModel(7424);
		RenderSystem.disableBlend();
		RenderSystem.enableTexture();
		IRenderTypeBuffer.Impl renderTypeBuffer = IRenderTypeBuffer.getImpl(Tessellator.getInstance().getBuffer());
		matrixStack.translate(0.0D, 0.0D, 400.0D);

		writeTooltipLines(tooltipToRender, font, (float) leftX, topY, matrix4f, renderTypeBuffer);

		renderTypeBuffer.finish();
		matrixStack.pop();

		tooltipToRender = Collections.emptyList();
		tooltipRenderFont = null;
	}

	private static int getMaxLineWidth(List<? extends IReorderingProcessor> tooltips, FontRenderer font) {
		int maxLineWidth = 0;
		for (IReorderingProcessor ireorderingprocessor : tooltips) {
			int lineWidth = font.func_243245_a(ireorderingprocessor);
			if (lineWidth > maxLineWidth) {
				maxLineWidth = lineWidth;
			}
		}
		return maxLineWidth;
	}

	private static void writeTooltipLines(List<? extends IReorderingProcessor> tooltips, FontRenderer font, float leftX, int topY, Matrix4f matrix4f, IRenderTypeBuffer.Impl renderTypeBuffer) {
		for (int i = 0; i < tooltips.size(); ++i) {
			IReorderingProcessor reorderingProcessor = tooltips.get(i);
			if (reorderingProcessor != null) {
				font.func_238416_a_(reorderingProcessor, leftX, (float) topY, -1, true, matrix4f, renderTypeBuffer, false, 0, 15728880);
			}

			if (i == 0) {
				topY += 2;
			}

			topY += 10;
		}
	}

	private static void fillGradient(Matrix4f matrix, BufferBuilder builder, int x1, int y1, int x2, int y2, int z, int colorA, int colorB) {
		float f = (float) (colorA >> 24 & 255) / 255.0F;
		float f1 = (float) (colorA >> 16 & 255) / 255.0F;
		float f2 = (float) (colorA >> 8 & 255) / 255.0F;
		float f3 = (float) (colorA & 255) / 255.0F;
		float f4 = (float) (colorB >> 24 & 255) / 255.0F;
		float f5 = (float) (colorB >> 16 & 255) / 255.0F;
		float f6 = (float) (colorB >> 8 & 255) / 255.0F;
		float f7 = (float) (colorB & 255) / 255.0F;
		builder.pos(matrix, (float) x2, (float) y1, (float) z).color(f1, f2, f3, f).endVertex();
		builder.pos(matrix, (float) x1, (float) y1, (float) z).color(f1, f2, f3, f).endVertex();
		builder.pos(matrix, (float) x1, (float) y2, (float) z).color(f5, f6, f7, f4).endVertex();
		builder.pos(matrix, (float) x2, (float) y2, (float) z).color(f5, f6, f7, f4).endVertex();
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, Dimension dimension, Position offset, ITextComponent... tooltip) {
		return getButtonStateData(uv, dimension, offset, Arrays.asList(tooltip));
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, String tooltip, Dimension dimension) {
		return getButtonStateData(uv, tooltip, dimension, new Position(0, 0));
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, String tooltip, Dimension dimension, Position offset) {
		return new ToggleButton.StateData(new TextureBlitData(GUI_CONTROLS, offset, Dimension.SQUARE_256, uv, dimension),
				new TranslationTextComponent(tooltip)
		);
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, Dimension dimension, Position offset, List<? extends ITextComponent> tooltip) {
		return new ToggleButton.StateData(new TextureBlitData(GUI_CONTROLS, offset, Dimension.SQUARE_256, uv, dimension), tooltip);
	}

	public static void renderSlotsBackground(Minecraft minecraft, MatrixStack matrixStack, int x, int y, int slotsInRow, int fullSlotRows, int extraRowSlots) {
		renderSlotsBackground(minecraft, matrixStack, x, y, slotsInRow, fullSlotRows);
		if (extraRowSlots > 0) {
			renderSlotsBackground(minecraft, matrixStack, x, y + fullSlotRows * 18, extraRowSlots, 1);
		}
	}
}
