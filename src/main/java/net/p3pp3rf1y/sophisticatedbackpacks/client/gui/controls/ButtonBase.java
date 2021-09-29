package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import net.minecraft.client.Minecraft;
import net.minecraft.client.resources.sounds.SimpleSoundInstance;
import net.minecraft.sounds.SoundEvents;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.function.IntConsumer;

public abstract class ButtonBase extends BackpackWidget {
	protected IntConsumer onClick;

	protected ButtonBase(Position position, Dimension dimension, IntConsumer onClick) {
		super(position, dimension);
		this.onClick = onClick;
	}

	protected void setOnClick(IntConsumer onClick) {
		this.onClick = onClick;
	}

	@Override
	public boolean mouseClicked(double mouseX, double mouseY, int button) {
		if (!isMouseOver(mouseX, mouseY)) {
			return false;
		}
		onClick.accept(button);
		if (Boolean.TRUE.equals(Config.CLIENT.playButtonSound.get())) {
			Minecraft.getInstance().getSoundManager().play(SimpleSoundInstance.forUI(SoundEvents.UI_BUTTON_CLICK, 1.0F));
		}
		return true;
	}
}
