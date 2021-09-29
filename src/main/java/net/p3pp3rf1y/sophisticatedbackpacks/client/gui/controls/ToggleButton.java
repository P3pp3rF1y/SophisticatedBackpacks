package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.FormattedText;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;

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
		GuiHelper.blit(minecraft, matrixStack, x, y, data.getTexture());
		if (isMouseOver(mouseX, mouseY)) {
			GuiHelper.setTooltipToRender(data.getTooltip());
		}
	}

	public static class StateData {
		private final TextureBlitData texture;
		private final List<? extends FormattedText> tooltip;

		public StateData(TextureBlitData texture, List<? extends Component> tooltip) {
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

		public List<? extends FormattedText> getTooltip() {
			return tooltip;
		}
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration for toggle button - should be custom for every type with usage saying to click to toggle to next state
	}
}
