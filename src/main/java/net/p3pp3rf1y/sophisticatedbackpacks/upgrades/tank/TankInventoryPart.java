package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.BufferBuilder;
import com.mojang.blaze3d.vertex.BufferUploader;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.Tesselator;
import com.mojang.blaze3d.vertex.VertexFormat;
import com.mojang.math.Matrix4f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.network.chat.FormattedText;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeInventoryPartBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModFluids;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.XpHelper;

import java.util.ArrayList;
import java.util.List;

public class TankInventoryPart extends UpgradeInventoryPartBase<TankUpgradeContainer> {
	private static final TextureBlitData OVERLAY = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(47, 30), new Dimension(16, 18));
	private final Position pos;
	private final int height;
	private final BackpackScreen screen;

	public TankInventoryPart(int upgradeSlot, TankUpgradeContainer container, Position pos, int height, BackpackScreen screen) {
		super(upgradeSlot, container);
		this.pos = pos;
		this.height = height;
		this.screen = screen;
	}

	@Override
	public void render(PoseStack matrixStack, int mouseX, int mouseY) {
		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.y(), GuiHelper.BAR_BACKGROUND_TOP);
		int yOffset = 18;
		for (int i = 0; i < (height - 36) / 18; i++) {
			GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.y() + yOffset, GuiHelper.BAR_BACKGROUND_MIDDLE);
			yOffset += 18;
		}
		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.y() + yOffset, GuiHelper.BAR_BACKGROUND_BOTTOM);

		renderFluid(matrixStack, mouseX, mouseY);

		yOffset = 0;
		for (int i = 0; i < height / 18; i++) {
			GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft() + 1, pos.y() + yOffset, OVERLAY);
			yOffset += 18;
		}
	}

	private int getTankLeft() {
		return pos.x() + 9;
	}

	@Override
	public boolean handleMouseReleased(double mouseX, double mouseY, int button) {
		if (mouseX < screen.getGuiLeft() + getTankLeft() || mouseX >= screen.getGuiLeft() + getTankLeft() + 18 ||
				mouseY < screen.getGuiTop() + pos.y() || mouseY >= screen.getGuiTop() + pos.y() + height) {
			return false;
		}

		ItemStack cursorStack = screen.getMenu().getCarried();
		if (!cursorStack.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).isPresent()) {
			return false;
		}

		PacketHandler.sendToServer(new TankClickMessage(upgradeSlot));

		return true;
	}

	@Override
	public void renderErrorOverlay(PoseStack matrixStack) {
		screen.renderOverlay(matrixStack, BackpackScreen.ERROR_SLOT_COLOR, getTankLeft() + 1, pos.y() + 1, 16, height - 2);
	}

	private void renderTooltip(int mouseX, int mouseY, FluidStack contents, int capacity) {
		int screenX = screen.getGuiLeft() + pos.x() + 10;
		int screenY = screen.getGuiTop() + pos.y() + 1;
		if (mouseX >= screenX && mouseX < screenX + 16 && mouseY >= screenY && mouseY < screenY + height - 2) {
			List<FormattedText> tooltip = new ArrayList<>();
			if (!contents.isEmpty()) {
				tooltip.add(contents.getDisplayName());
			}
			tooltip.add(getContentsTooltip(contents, capacity));
			GuiHelper.setTooltipToRender(tooltip);
		}
	}

	private TranslatableComponent getContentsTooltip(FluidStack contents, int capacity) {
		if (contents.getFluid().is(ModFluids.EXPERIENCE_TAG)) {
			double contentsLevels = XpHelper.getLevelsForExperience(contents.getAmount());
			double tankCapacityLevels = XpHelper.getLevelsForExperience(capacity);

			return new TranslatableComponent(TranslationHelper.translUpgradeKey("tank.xp_contents_tooltip"), String.format("%.1f", contentsLevels), String.format("%.1f", tankCapacityLevels));
		}
		return new TranslatableComponent(TranslationHelper.translUpgradeKey("tank.contents_tooltip"), String.format("%,d", contents.getAmount()), String.format("%,d", capacity));
	}

	private void renderFluid(PoseStack matrixStack, int mouseX, int mouseY) {
		FluidStack contents = container.getContents();

		int capacity = container.getTankCapacity();
		if (contents.isEmpty()) {
			renderTooltip(mouseX, mouseY, FluidStack.EMPTY, capacity);
			return;
		}

		Fluid fluid = contents.getFluid();
		int fill = contents.getAmount();
		int displayLevel = (int) ((height - 2) * ((float) fill / capacity));

		ResourceLocation texture = fluid.getAttributes().getStillTexture(contents);
		TextureAtlasSprite still = Minecraft.getInstance().getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(texture);
		renderTiledFluidTextureAtlas(matrixStack, still, fluid.getAttributes().getColor(), pos.x() + 10, pos.y() + 1 + height - 2 - displayLevel, displayLevel);
		renderTooltip(mouseX, mouseY, contents, capacity);
	}

	private void renderTiledFluidTextureAtlas(PoseStack matrixStack, TextureAtlasSprite sprite, int color, int x, int y, int height) {
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

}
