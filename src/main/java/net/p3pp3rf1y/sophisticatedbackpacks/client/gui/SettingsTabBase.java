package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.FormattedText;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Label;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.IntConsumer;

public abstract class SettingsTabBase<T extends AbstractContainerScreen<?>> extends Tab {
	private static final int RIGHT_BORDER_WIDTH = 6;
	private static final int BOTTOM_BORDER_HEIGHT = 7;

	protected final T screen;
	protected Dimension openTabDimension = new Dimension(0, 0);
	protected boolean isOpen = false;
	private Runnable onOpen = () -> {};
	private Runnable onClose = () -> {};
	private final List<BackpackWidget> hideableChildren = new ArrayList<>();
	private final List<FormattedText> openTooltip;

	protected SettingsTabBase(Position position, T screen, Component tabLabel, List<FormattedText> tooltip, List<FormattedText> openTooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		super(position, tooltip, getTabButton);
		this.screen = screen;
		this.openTooltip = openTooltip;
		addLabel(tabLabel);
	}

	private void addLabel(Component tabLabel) {
		addHideableChild(new Label(new Position(x + 20, y + 8), tabLabel));
	}

	protected SettingsTabBase(Position position, T screen, Component tabLabel, Component tooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		super(position, tooltip, getTabButton);
		this.screen = screen;
		addLabel(tabLabel);
		openTooltip = Collections.emptyList();
	}

	protected <U extends BackpackWidget> U addHideableChild(U widget) {
		hideableChildren.add(widget);
		updateOpenTabDimension(widget);
		return widget;
	}

	private <U extends BackpackWidget> void updateOpenTabDimension(U widget) {
		int widgetMaxWidthExtension = widget.getX() + widget.getWidth() + RIGHT_BORDER_WIDTH - x;
		int widgetMaxHeightExtension = widget.getY() + widget.getHeight() + BOTTOM_BORDER_HEIGHT - y;
		openTabDimension = new Dimension(Math.max(openTabDimension.width(), widgetMaxWidthExtension), Math.max(openTabDimension.height(), widgetMaxHeightExtension));
	}

	public void setHandlers(Runnable onOpen, Runnable onClose, BooleanSupplier shouldRender, BooleanSupplier shouldShowTooltip) {
		this.onOpen = onOpen;
		this.onClose = onClose;
		setHandlers(shouldShowTooltip, shouldRender);
	}

	@Override
	protected boolean isClosedTooltipVisible(int mouseX, int mouseY) {
		return !isOpen && super.isClosedTooltipVisible(mouseX, mouseY);
	}

	@Override
	public void render(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (!openTooltip.isEmpty() && isOpenTooltipVisible(mouseX, mouseY)) {
			GuiHelper.setTooltipToRender(openTooltip);
		}
		super.render(matrixStack, mouseX, mouseY, partialTicks);
	}

	private boolean isOpenTooltipVisible(int mouseX, int mouseY) {
		return isOpen && mouseX > x && mouseY > y + 3 && mouseX < x + 18 && mouseY < y + 21;
	}

	public void close() {
		setOpen(false);
		onTabClose();
	}

	protected void onTabOpen() {
		setWidth(openTabDimension.width());
		setHeight(openTabDimension.height());

		hideableChildren.forEach(this::addChild);
		onOpen.run();
	}

	protected void onTabClose() {
		setWidth(DEFAULT_WIDTH);
		setHeight(DEFAULT_HEIGHT);

		children.removeAll(hideableChildren);
		onClose.run();
	}

	protected void setOpen(boolean isOpen) {
		this.isOpen = isOpen;
		if (isOpen) {
			onTabOpen();
		} else {
			onTabClose();
		}
	}

	@Override
	protected void onTabIconClicked(int button) {
		if (button != 0) {
			return;
		}
		setOpen(!isOpen);
	}
}
