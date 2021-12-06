package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.gui.components.events.ContainerEventHandler;
import net.minecraft.client.gui.components.events.GuiEventListener;
import net.minecraft.client.gui.screens.Screen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

public abstract class CompositeBackpackWidget<T extends BackpackWidget> extends BackpackWidget implements ContainerEventHandler {
	protected final List<T> children = new ArrayList<>();

	private boolean dragging = false;

	@Nullable
	private GuiEventListener listener;

	protected CompositeBackpackWidget(Position position, Dimension dimension) {
		super(position, dimension);
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		children.forEach(child -> child.render(matrixStack, mouseX, mouseY, partialTicks));
	}

	protected <U extends T> U addChild(U child) {
		children.add(child);
		return child;
	}

	@Override
	public List<? extends GuiEventListener> children() {
		return children;
	}

	@Override
	public boolean isDragging() {
		for (T child : children) {
			if (child instanceof ContainerEventHandler containerEventHandler && containerEventHandler.isDragging()) {
				return true;
			}
		}
		return dragging;
	}

	@Override
	public boolean mouseClicked(double mouseX, double mouseY, int button) {
		return getChildAt(mouseX, mouseY).map(l -> {
			if (l.mouseClicked(mouseX, mouseY, button)) {
				setFocused(l);
				if (button == 0) {
					setDragging(true);
				}
				return true;
			}
			return false;
		}).orElse(false);
	}

	@Override
	public void setDragging(boolean dragging) {
		this.dragging = dragging;
	}

	@Nullable
	@Override
	public GuiEventListener getFocused() {
		return listener;
	}

	@Override
	public void setFocused(@Nullable GuiEventListener listener) {
		this.listener = listener;
	}

	@Override
	public void renderTooltip(Screen screen, PoseStack poseStack, int mouseX, int mouseY) {
		super.renderTooltip(screen, poseStack, mouseX, mouseY);
		children.forEach(c -> c.renderTooltip(screen, poseStack, mouseX, mouseY));
	}
}
