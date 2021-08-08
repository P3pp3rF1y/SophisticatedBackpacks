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
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.IntConsumer;

public abstract class Tab extends CompositeWidget<Widget> {
	private static final int TEXTURE_WIDTH = 256;
	private static final int TEXTURE_HEIGHT = 256;
	public static final int DEFAULT_HEIGHT = 24;
	protected static final int DEFAULT_WIDTH = 21;

	private int width = DEFAULT_WIDTH;
	private int height = DEFAULT_HEIGHT;
	private final List<ITextProperties> tooltip;

	private BooleanSupplier shouldShowTooltip = () -> true;
	private BooleanSupplier shouldRender = () -> true;

	protected Tab(Position position, List<ITextProperties> tooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		super(position);
		this.tooltip = tooltip;
		addChild(getTabButton.apply(this::onTabIconClicked));
	}

	protected Tab(Position position, ITextComponent tooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		this(position, ImmutableList.of(tooltip), getTabButton);
	}

	public void setHandlers(BooleanSupplier shouldShowTooltip, BooleanSupplier shouldRender) {
		this.shouldShowTooltip = shouldShowTooltip;
		this.shouldRender = shouldRender;
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (!shouldRender.getAsBoolean()) {
			return;
		}
		if (isClosedTooltipVisible(mouseX, mouseY)) {
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
		minecraft.getTextureManager().bind(GuiHelper.GUI_CONTROLS);

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
}
