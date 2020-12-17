package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.google.common.collect.ImmutableList;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Rectangle2d;
import net.minecraft.util.IReorderingProcessor;
import net.minecraft.util.text.ITextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ItemButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Label;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

@OnlyIn(Dist.CLIENT)
public abstract class UpgradeSettingsTab<C extends UpgradeContainerBase<?, ?>> extends CompositeWidget<Widget> {
	private static final int TEXTURE_WIDTH = 256;
	private static final int TEXTURE_HEIGHT = 256;
	public static final int DEFAULT_HEIGHT = 24;
	private static final int DEFAULT_WIDTH = 21;
	private static final int RIGHT_BORDER_WIDTH = 6;
	private static final int BOTTOM_BORDER_HEIGHT = 7;

	protected final BackpackScreen screen;
	private final List<IReorderingProcessor> closedTooltip;
	private int width = DEFAULT_WIDTH;
	private int height = DEFAULT_HEIGHT;
	private final C upgradeContainer;
	protected Dimension openTabDimension = new Dimension(0, 0);
	private Consumer<UpgradeSettingsTab<C>> onOpen = t -> {};
	private Consumer<UpgradeSettingsTab<C>> onClose = t -> {};
	private Predicate<UpgradeSettingsTab<C>> shouldRender = t -> true;
	private Predicate<UpgradeSettingsTab<C>> shouldShowTooltip = t -> true;
	private final List<Widget> hideableChildren = new ArrayList<>();

	protected UpgradeSettingsTab(C upgradeContainer, Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(position);
		this.upgradeContainer = upgradeContainer;
		this.closedTooltip = ImmutableList.of(closedTooltip.func_241878_f());
		addChild(new ItemButton(new Position(x + 1, y + 4), this::onTabIconClicked, getContainer().getUpgradeStack()) {
			@Override
			protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
				super.renderWidget(matrixStack, mouseX, mouseY, partialTicks);
				if (!upgradeContainer.isOpen() && shouldShowTooltip.test(UpgradeSettingsTab.this) && isMouseOver(mouseX, mouseY)) {
					GuiHelper.setTooltipToRender(UpgradeSettingsTab.this.closedTooltip);
				}
			}
		});
		addHideableChild(new Label(new Position(x + 20, y + 8), tabLabel));
		this.screen = screen;
		moveSlotsOutOfView();
	}

	public void setHandlers(Consumer<UpgradeSettingsTab<C>> onOpen, Consumer<UpgradeSettingsTab<C>> onClose, Predicate<UpgradeSettingsTab<C>> shouldRender,
			Predicate<UpgradeSettingsTab<C>> shouldShowTooltip) {
		this.onOpen = onOpen;
		this.onClose = onClose;
		this.shouldRender = shouldRender;
		this.shouldShowTooltip = shouldShowTooltip;
	}

	private void onTabIconClicked(int button) {
		if (button != 0) {
			return;
		}
		setOpen(!upgradeContainer.isOpen());
	}

	protected C getContainer() {
		return upgradeContainer;
	}

	protected <U extends Widget> U addHideableChild(U widget) {
		hideableChildren.add(widget);
		updateOpenTabDimension(widget);
		return widget;
	}

	private <U extends Widget> void updateOpenTabDimension(U widget) {
		int widgetMaxWidthExtension = widget.getX() + widget.getWidth() + RIGHT_BORDER_WIDTH - x;
		int widgetMaxHeightExtension = widget.getY() + widget.getHeight() + BOTTOM_BORDER_HEIGHT - y;
		openTabDimension = new Dimension(Math.max(openTabDimension.getWidth(), widgetMaxWidthExtension), Math.max(openTabDimension.getHeight(), widgetMaxHeightExtension));
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (!shouldRender.test(this)) {
			return;
		}
		super.render(matrixStack, mouseX, mouseY, partialTicks);
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		minecraft.getTextureManager().bindTexture(GuiHelper.UPGRADE_CONTROLS);

		int halfHeight = height / 2;
		blit(matrixStack, x, y, (float) TEXTURE_WIDTH - width, 0, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x, y + halfHeight, (float) TEXTURE_WIDTH - width, (float) TEXTURE_HEIGHT - halfHeight, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x - 3, y, TEXTURE_WIDTH / 2, TEXTURE_HEIGHT - height, 3, height);
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public int getHeight() {
		return height;
	}

	public int getTopY() {
		return y;
	}

	public int getBottomY() {
		return y + getHeight();
	}

	public void close() {
		setOpen(false);
		onTabClose();
	}

	protected void onTabOpen() {
		width = openTabDimension.getWidth();
		height = openTabDimension.getHeight();

		hideableChildren.forEach(this::addChild);
		onOpen.accept(this);

		moveSlotsToTab();
	}

	protected abstract void moveSlotsToTab();

	protected void moveSlotsOutOfView() {
		getContainer().getSlots().forEach(slot -> {
			slot.xPos = -100;
			slot.yPos = -100;
		});

	}

	protected void onTabClose() {
		width = DEFAULT_WIDTH;
		height = DEFAULT_HEIGHT;

		children.removeAll(hideableChildren);
		onClose.accept(this);

		moveSlotsOutOfView();
	}

	private void setOpen(boolean isOpen) {
		upgradeContainer.setIsOpen(isOpen);
		if (isOpen) {
			onTabOpen();
		} else {
			onTabClose();
		}
	}

	public Rectangle2d getRectangle() {
		return new Rectangle2d(x, y, width, height);
	}

	public void onAfterInit() {
		if (upgradeContainer.isOpen()) {
			setOpen(true);
		}
	}
}
