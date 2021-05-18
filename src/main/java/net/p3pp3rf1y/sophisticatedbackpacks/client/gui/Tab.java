package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.google.common.collect.ImmutableList;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Rectangle2d;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.ITextProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.List;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.Predicate;

public abstract class Tab extends CompositeWidget<Widget> {
	private static final int TEXTURE_WIDTH = 256;
	private static final int TEXTURE_HEIGHT = 256;
	public static final int DEFAULT_HEIGHT = 24;
	protected static final int DEFAULT_WIDTH = 21;

	private int width = DEFAULT_WIDTH;
	private int height = DEFAULT_HEIGHT;
	private final List<ITextProperties> tooltip;

	private Predicate<Tab> shouldShowTooltip = t -> true;
	private Predicate<Tab> shouldRender = t -> true;

	protected Tab(Position position, ITextComponent tooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		super(position);
		this.tooltip = ImmutableList.of(tooltip);
		addChild(getTabButton.apply(this::onTabIconClicked));
	}

	public void setHandlers(Predicate<Tab> shouldShowTooltip, Predicate<Tab> shouldRender) {
		this.shouldShowTooltip = shouldShowTooltip;
		this.shouldRender = shouldRender;
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (!shouldRender.test(this)) {
			return;
		}
		if (isTooltipVisible(mouseX, mouseY)) {
			GuiHelper.setTooltipToRender(tooltip);
		}
		super.render(matrixStack, mouseX, mouseY, partialTicks);
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

	public Rectangle2d getRectangle() {
		return new Rectangle2d(x, y, width, height);
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		minecraft.getTextureManager().bindTexture(GuiHelper.GUI_CONTROLS);

		int halfHeight = height / 2;
		blit(matrixStack, x, y, (float) TEXTURE_WIDTH - width, 0, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x, y + halfHeight, (float) TEXTURE_WIDTH - width, (float) TEXTURE_HEIGHT - halfHeight, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x - 3, y, TEXTURE_WIDTH / 2, TEXTURE_HEIGHT - height, 3, height);
	}

	protected boolean isTooltipVisible(int mouseX, int mouseY) {
		return shouldShowTooltip.test(this) && isMouseOver(mouseX, mouseY);
	}

	public int getTopY() {
		return y;
	}

	public int getBottomY() {
		return y + getHeight();
	}

	protected abstract void onTabIconClicked(int button);
}
