package net.p3pp3rf1y.sophisticatedcore.client.gui.controls;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.IntConsumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class ToggleButton<T extends Comparable<T>> extends Button {
	private final Map<T, StateData> stateData;
	private final Supplier<T> getState;

	public ToggleButton(Position position, ButtonDefinition.Toggle<T> buttonDefinition, IntConsumer onClick, Supplier<T> getState) {
		super(position, buttonDefinition, onClick);

		stateData = buttonDefinition.getStateData();
		this.getState = getState;
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		StateData data = stateData.get(getState.get());
		GuiHelper.blit(matrixStack, x, y, data.getTexture());
	}

	@Override
	public void renderTooltip(Screen screen, PoseStack poseStack, int mouseX, int mouseY) {
		if (isMouseOver(mouseX, mouseY)) {
			StateData data = stateData.get(getState.get());
			GuiHelper.renderTooltip(screen, poseStack, data.getTooltip(), mouseX, mouseY);
		}
	}

	public static class StateData {
		private final TextureBlitData texture;
		private final List<Component> tooltip;

		public StateData(TextureBlitData texture, List<Component> tooltip) {
			this.texture = texture;
			this.tooltip = tooltip;
		}

		public StateData(TextureBlitData texture, Component... tooltip) {
			this.texture = texture;
			this.tooltip = Arrays.stream(tooltip).collect(Collectors.toList());
		}

		public TextureBlitData getTexture() {
			return texture;
		}

		public List<Component> getTooltip() {
			return tooltip;
		}
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration for toggle button - should be custom for every type with usage saying to click to toggle to next state
	}
}
