package net.p3pp3rf1y.sophisticatedcore.client.gui.utils;

import com.mojang.blaze3d.platform.GlStateManager;
import com.mojang.blaze3d.platform.Lighting;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.BufferBuilder;
import com.mojang.blaze3d.vertex.BufferUploader;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.Tesselator;
import com.mojang.blaze3d.vertex.VertexFormat;
import com.mojang.datafixers.util.Either;
import com.mojang.math.Matrix4f;
import com.mojang.math.Vector3f;
import net.minecraft.CrashReport;
import net.minecraft.CrashReportCategory;
import net.minecraft.ReportedException;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.tooltip.ClientTooltipComponent;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.locale.Language;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.FormattedText;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.client.event.RenderTooltipEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;

import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GuiHelper {
	public static final ResourceLocation GUI_CONTROLS = SophisticatedCore.getRL("textures/gui/gui_controls.png");
	private static final int GUI_CONTROLS_TEXTURE_WIDTH = 256;
	private static final int GUI_CONTROLS_TEXTURE_HEIGHT = 256;
	public static final TextureBlitData BAR_BACKGROUND_BOTTOM = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 66), Dimension.SQUARE_18);
	public static final TextureBlitData BAR_BACKGROUND_MIDDLE = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 48), Dimension.SQUARE_18);
	public static final TextureBlitData BAR_BACKGROUND_TOP = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 30), Dimension.SQUARE_18);
	public static final ResourceLocation ICONS = SophisticatedCore.getRL("textures/gui/icons.png");
	public static final TextureBlitData CRAFTING_RESULT_SLOT = new TextureBlitData(GUI_CONTROLS, new UV(71, 216), new Dimension(26, 26));
	public static final TextureBlitData DEFAULT_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(47, 0), Dimension.SQUARE_18);
	public static final TextureBlitData DEFAULT_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(29, 0), Dimension.SQUARE_18);
	public static final TextureBlitData SMALL_BUTTON_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 18), Dimension.SQUARE_12);
	public static final TextureBlitData SMALL_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(41, 18), Dimension.SQUARE_12);
	public static final ResourceLocation SLOTS_BACKGROUND = SophisticatedCore.getRL("textures/gui/slots_background.png");

	private static final Map<Integer, TextureBlitData> SLOTS_BACKGROUNDS = new HashMap<>();

	private GuiHelper() {}

	public static void renderItemInGUI(PoseStack matrixStack, Minecraft minecraft, ItemStack stack, int xPosition, int yPosition) {
		renderItemInGUI(matrixStack, minecraft, stack, xPosition, yPosition, false);
	}

	public static void renderSlotsBackground(PoseStack matrixStack, int x, int y, int slotWidth, int slotHeight) {
		int key = getSlotsBackgroundKey(slotWidth, slotHeight);
		blit(matrixStack, x, y, SLOTS_BACKGROUNDS.computeIfAbsent(key, k ->
				new TextureBlitData(SLOTS_BACKGROUND, Dimension.SQUARE_256, new UV(0, 0), new Dimension(slotWidth * 18, slotHeight * 18))
		));
	}

	private static int getSlotsBackgroundKey(int slotWidth, int slotHeight) {
		return slotWidth * 31 + slotHeight;
	}

	public static void renderItemInGUI(PoseStack matrixStack, Minecraft minecraft, ItemStack stack, int xPosition, int yPosition, boolean renderOverlay) {
		renderItemInGUI(matrixStack, minecraft, stack, xPosition, yPosition, renderOverlay, null);
	}

	public static void renderItemInGUI(PoseStack matrixStack, Minecraft minecraft, ItemStack stack, int xPosition, int yPosition, boolean renderOverlay,
			@Nullable String countText) {
		ItemRenderer itemRenderer = minecraft.getItemRenderer();
		float originalZLevel = itemRenderer.blitOffset;
		itemRenderer.blitOffset += getZOffset(matrixStack);
		itemRenderer.renderAndDecorateItem(stack, xPosition, yPosition);
		if (renderOverlay) {
			itemRenderer.renderGuiItemDecorations(minecraft.font, stack, xPosition, yPosition, countText);
		}
		itemRenderer.blitOffset = originalZLevel;
	}

	private static int getZOffset(PoseStack matrixStack) {
		Float zOffset = ObfuscationReflectionHelper.getPrivateValue(Matrix4f.class, matrixStack.last().pose(), "f_27614_");
		return zOffset == null ? 0 : zOffset.intValue();
	}

	public static void blit(PoseStack matrixStack, int x, int y, TextureBlitData texData) {
		RenderSystem.setShader(GameRenderer::getPositionTexShader);
		RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
		RenderSystem.setShaderTexture(0, texData.getTextureName());
		GuiComponent.blit(matrixStack, x + texData.getXOffset(), y + texData.getYOffset(), texData.getU(), texData.getV(), texData.getWidth(), texData.getHeight(), texData.getTextureWidth(), texData.getTextureHeight());
	}

	public static void coloredBlit(Matrix4f matrix, int x, int y, TextureBlitData texData, int color) {
		float red = (color >> 16 & 255) / 255F;
		float green = (color >> 8 & 255) / 255F;
		float blue = (color & 255) / 255F;
		float alpha = (color >> 24 & 255) / 255F;

		int xMin = x + texData.getXOffset();
		int yMin = y + texData.getYOffset();
		int xMax = xMin + texData.getWidth();
		int yMax = yMin + texData.getHeight();

		float minU = (float) texData.getU() / texData.getTextureWidth();
		float maxU = minU + ((float) texData.getWidth() / texData.getTextureWidth());
		float minV = (float) texData.getV() / texData.getTextureHeight();
		float maxV = minV + ((float) texData.getHeight() / texData.getTextureWidth());

		RenderSystem.setShader(GameRenderer::getPositionColorTexShader);
		BufferBuilder bufferbuilder = Tesselator.getInstance().getBuilder();
		bufferbuilder.begin(VertexFormat.Mode.QUADS, DefaultVertexFormat.POSITION_COLOR_TEX);
		bufferbuilder.vertex(matrix, xMin, yMax, 0).color(red, green, blue, alpha).uv(minU, maxV).endVertex();
		bufferbuilder.vertex(matrix, xMax, yMax, 0).color(red, green, blue, alpha).uv(maxU, maxV).endVertex();
		bufferbuilder.vertex(matrix, xMax, yMin, 0).color(red, green, blue, alpha).uv(maxU, minV).endVertex();
		bufferbuilder.vertex(matrix, xMin, yMin, 0).color(red, green, blue, alpha).uv(minU, minV).endVertex();
		bufferbuilder.end();
		BufferUploader.end(bufferbuilder);
	}

	public static void renderTooltipBackground(Matrix4f matrix4f, int tooltipWidth, int leftX, int topY, int tooltipHeight, int backgroundColor, int borderColorStart, int borderColorEnd) {
		Tesselator tessellator = Tesselator.getInstance();
		BufferBuilder bufferbuilder = tessellator.getBuilder();
		RenderSystem.setShader(GameRenderer::getPositionColorShader);
		bufferbuilder.begin(VertexFormat.Mode.QUADS, DefaultVertexFormat.POSITION_COLOR);

		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 4, leftX + tooltipWidth + 3, topY - 3, backgroundColor, backgroundColor);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY + tooltipHeight + 3, leftX + tooltipWidth + 3, topY + tooltipHeight + 4, backgroundColor, backgroundColor);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 3, leftX + tooltipWidth + 3, topY + tooltipHeight + 3, backgroundColor, backgroundColor);
		fillGradient(matrix4f, bufferbuilder, leftX - 4, topY - 3, leftX - 3, topY + tooltipHeight + 3, backgroundColor, backgroundColor);
		fillGradient(matrix4f, bufferbuilder, leftX + tooltipWidth + 3, topY - 3, leftX + tooltipWidth + 4, topY + tooltipHeight + 3, backgroundColor, backgroundColor);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 3 + 1, leftX - 3 + 1, topY + tooltipHeight + 3 - 1, borderColorStart, borderColorEnd);
		fillGradient(matrix4f, bufferbuilder, leftX + tooltipWidth + 2, topY - 3 + 1, leftX + tooltipWidth + 3, topY + tooltipHeight + 3 - 1, borderColorStart, borderColorEnd);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY - 3, leftX + tooltipWidth + 3, topY - 3 + 1, borderColorStart, borderColorStart);
		fillGradient(matrix4f, bufferbuilder, leftX - 3, topY + tooltipHeight + 2, leftX + tooltipWidth + 3, topY + tooltipHeight + 3, borderColorEnd, borderColorEnd);
		RenderSystem.enableDepthTest();
		RenderSystem.disableTexture();
		RenderSystem.enableBlend();
		RenderSystem.defaultBlendFunc();
		bufferbuilder.end();
		BufferUploader.end(bufferbuilder);
		RenderSystem.disableBlend();
		RenderSystem.enableTexture();
	}

	public static void writeTooltipLines(List<? extends FormattedText> textLines, Font font, float leftX, int topY, Matrix4f matrix4f, MultiBufferSource.BufferSource renderTypeBuffer, int color) {
		for (int i = 0; i < textLines.size(); ++i) {
			FormattedText line = textLines.get(i);
			if (line != null) {
				font.drawInBatch(Language.getInstance().getVisualOrder(line), leftX, topY, color, true, matrix4f, renderTypeBuffer, false, 0, 15728880);
			}

			if (i == 0) {
				topY += 2;
			}

			topY += 10;
		}
	}

	private static void fillGradient(Matrix4f matrix, BufferBuilder builder, int x1, int y1, int x2, int y2, int colorA, int colorB) {
		float f = (colorA >> 24 & 255) / 255.0F;
		float f1 = (colorA >> 16 & 255) / 255.0F;
		float f2 = (colorA >> 8 & 255) / 255.0F;
		float f3 = (colorA & 255) / 255.0F;
		float f4 = (colorB >> 24 & 255) / 255.0F;
		float f5 = (colorB >> 16 & 255) / 255.0F;
		float f6 = (colorB >> 8 & 255) / 255.0F;
		float f7 = (colorB & 255) / 255.0F;
		builder.vertex(matrix, x2, y1, 400).color(f1, f2, f3, f).endVertex();
		builder.vertex(matrix, x1, y1, 400).color(f1, f2, f3, f).endVertex();
		builder.vertex(matrix, x1, y2, 400).color(f5, f6, f7, f4).endVertex();
		builder.vertex(matrix, x2, y2, 400).color(f5, f6, f7, f4).endVertex();
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, Dimension dimension, Position offset, Component... tooltip) {
		return getButtonStateData(uv, dimension, offset, Arrays.asList(tooltip));
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, String tooltip, Dimension dimension) {
		return getButtonStateData(uv, tooltip, dimension, new Position(0, 0));
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, String tooltip, Dimension dimension, Position offset) {
		return new ToggleButton.StateData(new TextureBlitData(ICONS, offset, Dimension.SQUARE_256, uv, dimension),
				new TranslatableComponent(tooltip)
		);
	}

	public static ToggleButton.StateData getButtonStateData(UV uv, Dimension dimension, Position offset, List<Component> tooltip) {
		return new ToggleButton.StateData(new TextureBlitData(ICONS, offset, Dimension.SQUARE_256, uv, dimension), tooltip);
	}

	public static void renderSlotsBackground(PoseStack matrixStack, int x, int y, int slotsInRow, int fullSlotRows, int extraRowSlots) {
		renderSlotsBackground(matrixStack, x, y, slotsInRow, fullSlotRows);
		if (extraRowSlots > 0) {
			renderSlotsBackground(matrixStack, x, y + fullSlotRows * 18, extraRowSlots, 1);
		}
	}

	public static void renderTiledFluidTextureAtlas(PoseStack matrixStack, TextureAtlasSprite sprite, int color, int x, int y, int height) {
		RenderSystem.setShader(GameRenderer::getPositionTexColorShader);
		RenderSystem.setShaderTexture(0, sprite.atlas().location());
		BufferBuilder builder = Tesselator.getInstance().getBuilder();
		builder.begin(VertexFormat.Mode.QUADS, DefaultVertexFormat.POSITION_TEX_COLOR);

		float u1 = sprite.getU0();
		float v1 = sprite.getV0();
		int spriteHeight = sprite.getHeight();
		int spriteWidth = sprite.getWidth();
		int startY = y;
		float red = (color >> 16 & 255) / 255.0F;
		float green = (color >> 8 & 255) / 255.0F;
		float blue = (color & 255) / 255.0F;
		do {
			int renderHeight = Math.min(spriteHeight, height);
			height -= renderHeight;
			float v2 = sprite.getV((16f * renderHeight) / spriteHeight);

			// we need to draw the quads per width too
			Matrix4f matrix = matrixStack.last().pose();
			float u2 = sprite.getU((16f * 16) / spriteWidth);
			builder.vertex(matrix, x, (float) startY + renderHeight, 100).uv(u1, v2).color(red, green, blue, 1).endVertex();
			builder.vertex(matrix, (float) x + 16, (float) startY + renderHeight, 100).uv(u2, v2).color(red, green, blue, 1).endVertex();
			builder.vertex(matrix, (float) x + 16, startY, 100).uv(u2, v1).color(red, green, blue, 1).endVertex();
			builder.vertex(matrix, x, startY, 100).uv(u1, v1).color(red, green, blue, 1).endVertex();

			startY += renderHeight;
		} while (height > 0);

		// finish drawing sprites
		builder.end();
		BufferUploader.end(builder);
	}

	public static void renderControlBackground(PoseStack matrixStack, int x, int y, int renderWidth, int renderHeight) {
		RenderSystem.setShader(GameRenderer::getPositionTexShader);
		RenderSystem.setShaderTexture(0, GuiHelper.GUI_CONTROLS);

		int u = 29;
		int v = 146;
		int textureBgWidth = 66;
		int textureBgHeight = 56;
		int halfWidth = renderWidth / 2;
		int halfHeight = renderHeight / 2;
		GuiComponent.blit(matrixStack, x, y, u, v, halfWidth, halfHeight, GUI_CONTROLS_TEXTURE_WIDTH, GUI_CONTROLS_TEXTURE_HEIGHT);
		GuiComponent.blit(matrixStack, x, y + halfHeight, u, (float) v + textureBgHeight - halfHeight, halfWidth, halfHeight, GUI_CONTROLS_TEXTURE_WIDTH, GUI_CONTROLS_TEXTURE_HEIGHT);
		GuiComponent.blit(matrixStack, x + halfWidth, y, (float) u + textureBgWidth - halfWidth, v, halfWidth, halfHeight, GUI_CONTROLS_TEXTURE_WIDTH, GUI_CONTROLS_TEXTURE_HEIGHT);
		GuiComponent.blit(matrixStack, x + halfWidth, y + halfHeight, (float) u + textureBgWidth - halfWidth, (float) v + textureBgHeight - halfHeight, halfWidth, halfHeight, GUI_CONTROLS_TEXTURE_WIDTH, GUI_CONTROLS_TEXTURE_HEIGHT);
	}

	public interface ITooltipRenderPart {
		ITooltipRenderPart EMPTY = new ITooltipRenderPart() {
			@Override
			public int getWidth() {
				return 0;
			}

			@Override
			public int getHeight() {
				return 0;
			}

			@Override
			public void render(PoseStack matrixStack, int leftX, int topY, Font font) {
				//noop
			}
		};

		int getWidth();

		int getHeight();

		void render(PoseStack matrixStack, int leftX, int topY, Font font);
	}

	public static void tryRenderGuiItem(ItemRenderer itemRenderer, TextureManager textureManager,
			@Nullable LivingEntity livingEntity, ItemStack stack, int x, int y, int rotation) {
		if (!stack.isEmpty()) {
			BakedModel bakedmodel = itemRenderer.getModel(stack, null, livingEntity, 0);
			itemRenderer.blitOffset += 50.0F;

			try {
				renderGuiItem(itemRenderer, textureManager, stack, x, y, bakedmodel, rotation);
			}
			catch (Throwable throwable) {
				CrashReport crashreport = CrashReport.forThrowable(throwable, "Rendering item");
				CrashReportCategory crashreportcategory = crashreport.addCategory("Item being rendered");
				crashreportcategory.setDetail("Item Type", () -> String.valueOf(stack.getItem()));
				crashreportcategory.setDetail("Registry Name", () -> String.valueOf(stack.getItem().getRegistryName()));
				crashreportcategory.setDetail("Item Damage", () -> String.valueOf(stack.getDamageValue()));
				crashreportcategory.setDetail("Item NBT", () -> String.valueOf(stack.getTag()));
				crashreportcategory.setDetail("Item Foil", () -> String.valueOf(stack.hasFoil()));
				throw new ReportedException(crashreport);
			}

			itemRenderer.blitOffset -= 50.0F;
		}
	}

	private static void renderGuiItem(ItemRenderer itemRenderer, TextureManager textureManager, ItemStack stack, int x, int y, BakedModel bakedModel, int rotation) {
		textureManager.getTexture(InventoryMenu.BLOCK_ATLAS).setFilter(false, false);
		RenderSystem.setShaderTexture(0, InventoryMenu.BLOCK_ATLAS);
		RenderSystem.enableBlend();
		RenderSystem.blendFunc(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA);
		RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
		PoseStack posestack = RenderSystem.getModelViewStack();
		posestack.pushPose();
		posestack.translate(x, y, 100.0F + itemRenderer.blitOffset);
		posestack.translate(8.0D, 8.0D, 0.0D);
		if (rotation != 0) {
			posestack.mulPose(Vector3f.ZP.rotationDegrees(rotation));
		}
		posestack.scale(1.0F, -1.0F, 1.0F);
		posestack.scale(16.0F, 16.0F, 16.0F);
		RenderSystem.applyModelViewMatrix();
		PoseStack posestack1 = new PoseStack();
		MultiBufferSource.BufferSource bufferSource = Minecraft.getInstance().renderBuffers().bufferSource();
		boolean flag = !bakedModel.usesBlockLight();
		if (flag) {
			Lighting.setupForFlatItems();
		}

		itemRenderer.render(stack, ItemTransforms.TransformType.GUI, false, posestack1, bufferSource, 15728880, OverlayTexture.NO_OVERLAY, bakedModel);
		bufferSource.endBatch();
		RenderSystem.enableDepthTest();
		if (flag) {
			Lighting.setupFor3DItems();
		}

		posestack.popPose();
		RenderSystem.applyModelViewMatrix();
	}

	private static final Field TOOLTIP_FONT = ObfuscationReflectionHelper.findField(Screen.class, "tooltipFont");

	@Nullable
	private static Font getTooltipFont(Screen screen) {
		try {
			return (Font) TOOLTIP_FONT.get(screen);
		}
		catch (IllegalAccessException e) {
			SophisticatedCore.LOGGER.error("Unable to get value from field tooltipFont in Screen class: ", e);
			return null;
		}
	}

	public static void renderTooltip(Screen screen, PoseStack poseStack, List<Component> components, int x, int y) {
		List<ClientTooltipComponent> list = gatherTooltipComponents(components, x, screen.width, screen.height, getTooltipFont(screen), screen.font);
		screen.renderTooltipInternal(poseStack, list, x, y);
	}

	//copy of ForgeHooksClient.gatherTooltipComponents with splitting always called so that new lines in translation are properly wrapped
	public static List<ClientTooltipComponent> gatherTooltipComponents(List<? extends FormattedText> textElements, int mouseX, int screenWidth, int screenHeight,
			@Nullable Font forcedFont, Font fallbackFont) {
		Font font = ForgeHooksClient.getTooltipFont(forcedFont, ItemStack.EMPTY, fallbackFont);
		List<Either<FormattedText, TooltipComponent>> elements = textElements.stream()
				.map((Function<FormattedText, Either<FormattedText, TooltipComponent>>) Either::left)
				.collect(Collectors.toCollection(ArrayList::new));

		var event = new RenderTooltipEvent.GatherComponents(ItemStack.EMPTY, screenWidth, screenHeight, elements, -1);
		MinecraftForge.EVENT_BUS.post(event);
		if (event.isCanceled()) {
			return List.of();
		}

		// text wrapping
		int tooltipTextWidth = event.getTooltipElements().stream()
				.mapToInt(either -> either.map(font::width, component -> 0))
				.max()
				.orElse(0);

		int tooltipX = mouseX + 12;
		if (tooltipX + tooltipTextWidth + 4 > screenWidth) {
			tooltipX = mouseX - 16 - tooltipTextWidth;
			if (tooltipX < 4) // if the tooltip doesn't fit on the screen
			{
				if (mouseX > screenWidth / 2) {
					tooltipTextWidth = mouseX - 12 - 8;
				} else {
					tooltipTextWidth = screenWidth - 16 - mouseX;
				}
			}
		}

		if (event.getMaxWidth() > 0 && tooltipTextWidth > event.getMaxWidth()) {
			tooltipTextWidth = event.getMaxWidth();
		}

		int tooltipTextWidthF = tooltipTextWidth;
		return event.getTooltipElements().stream()
				.flatMap(either -> either.map(
						text -> font.split(text, tooltipTextWidthF).stream().map(ClientTooltipComponent::create),
						component -> Stream.of(ClientTooltipComponent.create(component))
				))
				.toList();
	}

	public static Optional<Rect2i> getPositiveRectangle(int x, int y, int width, int height) {
		if (x + width <= 0 || y + height <= 0) {
			return Optional.empty();
		}

		int positiveX = Math.max(0, x);
		int positiveY = Math.max(0, y);
		int positiveWidth = width + Math.min(0, x);
		int positiveHeight = height + Math.min(0, y);

		return Optional.of(new Rect2i(positiveX, positiveY, positiveWidth, positiveHeight));
	}
}
