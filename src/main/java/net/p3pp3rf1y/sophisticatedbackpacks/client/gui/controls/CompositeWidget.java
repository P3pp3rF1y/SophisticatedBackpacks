package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.gui.IGuiEventListener;
import net.minecraft.client.gui.INestedGuiEventHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

public abstract class CompositeWidget<T extends Widget> extends Widget implements INestedGuiEventHandler {
	protected final List<T> children = new ArrayList<>();

	private boolean dragging = false;

	@Nullable
	private IGuiEventListener listener;

	protected CompositeWidget(Position position) {
		super(position);
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		children.forEach(child -> child.render(matrixStack, mouseX, mouseY, partialTicks));
	}

	protected <U extends T> U addChild(U child) {
		children.add(child);
		return child;
	}

	@Override
	public List<? extends IGuiEventListener> children() {
		return children;
	}

	@Override
	public boolean isDragging() {
		for (T child : children) {
			if ((child instanceof INestedGuiEventHandler) && ((INestedGuiEventHandler) child).isDragging()) {
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
	public IGuiEventListener getFocused() {
		return listener;
	}

	@Override
	public void setFocused(@Nullable IGuiEventListener listener) {
		this.listener = listener;
	}

	@Override
	public void afterScreenRender(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		super.afterScreenRender(matrixStack, mouseX, mouseY, partialTicks);
		children.forEach(c -> c.afterScreenRender(matrixStack, mouseX, mouseY, partialTicks));
	}
}
