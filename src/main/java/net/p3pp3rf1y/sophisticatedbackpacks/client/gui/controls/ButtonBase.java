package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import net.minecraft.client.Minecraft;
import net.minecraft.client.audio.SimpleSound;
import net.minecraft.util.SoundEvents;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;

import java.util.function.Consumer;

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
		if (!isMouseOver(mouseX, mouseY)) {
			return false;
		}
		onClick.accept(button);
		if (Boolean.TRUE.equals(Config.CLIENT.playButtonSound.get())) {
			Minecraft.getInstance().getSoundHandler().play(SimpleSound.master(SoundEvents.UI_BUTTON_CLICK, 1.0F));
		}
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
