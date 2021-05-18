package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Label;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.Predicate;

public abstract class SettingsTab extends Tab {
	private static final int RIGHT_BORDER_WIDTH = 6;
	private static final int BOTTOM_BORDER_HEIGHT = 7;

	protected final BackpackScreen screen;
	protected Dimension openTabDimension = new Dimension(0, 0);
	protected boolean isOpen = false;
	private Consumer<SettingsTab> onOpen = t -> {};
	private Consumer<SettingsTab> onClose = t -> {};
	private final List<Widget> hideableChildren = new ArrayList<>();

	protected SettingsTab(Position position, BackpackScreen screen, ITextComponent tabLabel, ITextComponent tooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		super(position, tooltip, getTabButton);
		this.screen = screen;
		addHideableChild(new Label(new Position(x + 20, y + 8), tabLabel));
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

	public void setHandlers(Consumer<SettingsTab> onOpen, Consumer<SettingsTab> onClose, Predicate<Tab> shouldRender, Predicate<Tab> shouldShowTooltip) {
		this.onOpen = onOpen;
		this.onClose = onClose;
		setHandlers(shouldShowTooltip, shouldRender);
	}

	@Override
	protected boolean isTooltipVisible(int mouseX, int mouseY) {
		return !isOpen && super.isTooltipVisible(mouseX, mouseY);
	}

	public void close() {
		setOpen(false);
		onTabClose();
	}

	protected void onTabOpen() {
		setWidth(openTabDimension.getWidth());
		setHeight(openTabDimension.getHeight());

		hideableChildren.forEach(this::addChild);
		onOpen.accept(this);
	}

	protected void onTabClose() {
		setWidth(DEFAULT_WIDTH);
		setHeight(DEFAULT_HEIGHT);

		children.removeAll(hideableChildren);
		onClose.accept(this);
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
