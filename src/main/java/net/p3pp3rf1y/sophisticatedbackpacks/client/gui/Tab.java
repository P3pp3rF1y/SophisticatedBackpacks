package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeBackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.List;
import java.util.Optional;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.IntConsumer;

public abstract class Tab extends CompositeBackpackWidget<BackpackWidget> {
	private static final int TEXTURE_WIDTH = 256;
	private static final int TEXTURE_HEIGHT = 256;
	public static final int DEFAULT_HEIGHT = 24;
	protected static final int DEFAULT_WIDTH = 21;

	private int width = DEFAULT_WIDTH;
	private int height = DEFAULT_HEIGHT;
	private final List<Component> tooltip;

	private BooleanSupplier shouldShowTooltip = () -> true;
	private BooleanSupplier shouldRender = () -> true;

	protected Tab(Position position, List<Component> tooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		super(position, new Dimension(0, 0));
		this.tooltip = tooltip;
		addChild(getTabButton.apply(this::onTabIconClicked));
	}

	protected Tab(Position position, Component tooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		this(position, List.of(tooltip), getTabButton);
	}

	public void setHandlers(BooleanSupplier shouldShowTooltip, BooleanSupplier shouldRender) {
		this.shouldShowTooltip = shouldShowTooltip;
		this.shouldRender = shouldRender;
	}

	@Override
	public void render(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (!shouldRender.getAsBoolean()) {
			return;
		}
		super.render(matrixStack, mouseX, mouseY, partialTicks);
	}

	@Override
	public void renderTooltip(Screen screen, PoseStack poseStack, int mouseX, int mouseY) {
		super.renderTooltip(screen, poseStack, mouseX, mouseY);
		if (shouldRender.getAsBoolean() && isClosedTooltipVisible(mouseX, mouseY)) {
			screen.renderTooltip(poseStack, tooltip, Optional.empty(), mouseX, mouseY);
		}
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public int getHeight() {
		return height;
	}

	public void setWidth(int width) {
		this.width = width;
	}

	public void setHeight(int height) {
		this.height = height;
	}

	public Rect2i getRectangle() {
		return new Rect2i(x, y, width, height);
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		RenderSystem.setShader(GameRenderer::getPositionTexShader);
		RenderSystem.setShaderTexture(0, GuiHelper.GUI_CONTROLS);

		int halfHeight = height / 2;
		blit(matrixStack, x, y, (float) TEXTURE_WIDTH - width, 0, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x, y + halfHeight, (float) TEXTURE_WIDTH - width, (float) TEXTURE_HEIGHT - halfHeight, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x - 3, y, TEXTURE_WIDTH / 2, TEXTURE_HEIGHT - height, 3, height);
	}

	protected boolean isClosedTooltipVisible(int mouseX, int mouseY) {
		return shouldShowTooltip.getAsBoolean() && isMouseOver(mouseX, mouseY);
	}

	public int getTopY() {
		return y;
	}

	public int getBottomY() {
		return y + getHeight();
	}

	protected abstract void onTabIconClicked(int button);

	@Override
	public NarrationPriority narrationPriority() {
		return NarrationPriority.NONE;
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//noop
	}
}
