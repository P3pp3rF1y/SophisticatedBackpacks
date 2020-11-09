package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import java.util.Map;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class ToggleButton<T extends Comparable<T>> extends Button {

	private final Map<T, TextureBlitData> stateTextures;
	private final Supplier<T> getState;

	public ToggleButton(int x, int y, int width, int height, Predicate<Integer> onClick, TextureBlitData backgroundTexture, Map<T, TextureBlitData> stateTextures, Supplier<T> getState) {
		super(x, y, width, height, onClick, backgroundTexture);

		this.stateTextures = stateTextures;
		this.getState = getState;
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		TextureBlitData texture = stateTextures.get(getState.get());
		GuiHelper.blit(minecraft, matrixStack, x, y, texture);
	}
}
