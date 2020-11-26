package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;

import java.util.function.Consumer;

@OnlyIn(Dist.CLIENT)
public abstract class ButtonBase extends Widget {
	protected final int width;
	protected final int height;
	protected final Consumer<Integer> onClick;

	protected ButtonBase(Position position, Dimension dimension, Consumer<Integer> onClick) {
		super(position);
		width = dimension.getWidth();
		height = dimension.getHeight();
		this.onClick = onClick;
	}

	@Override
	public boolean mouseClicked(double mouseX, double mouseY, int button) {
		onClick.accept(button);
		return true;
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public int getHeight() {
		return height;
	}
}
