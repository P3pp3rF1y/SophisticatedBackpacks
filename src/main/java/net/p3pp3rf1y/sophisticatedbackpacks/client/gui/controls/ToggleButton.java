package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.util.IReorderingProcessor;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class ToggleButton<T extends Comparable<T>> extends Button {

	private final Map<T, StateData> stateData;
	private final Supplier<T> getState;

	public ToggleButton(int x, int y, int width, int height, Predicate<Integer> onClick, TextureBlitData backgroundTexture, Map<T, StateData> stateData, Supplier<T> getState) {
		super(x, y, width, height, onClick, backgroundTexture);

		this.stateData = stateData;
		this.getState = getState;
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		StateData data = stateData.get(getState.get());
		GuiHelper.blit(minecraft, matrixStack, x, y, data.getTexture());
		if (isMouseOver(mouseX, mouseY)) {
			GuiHelper.setTooltipToRender(data.getTooltip());
		}
	}

	public static class StateData {
		private final TextureBlitData texture;
		private final List<IReorderingProcessor> tooltip;

		public StateData(TextureBlitData texture, ITextComponent tooltip) {
			this.texture = texture;
			this.tooltip = Collections.singletonList(tooltip.func_241878_f());
		}

		public TextureBlitData getTexture() {
			return texture;
		}

		public List<IReorderingProcessor> getTooltip() {
			return tooltip;
		}
	}
}
