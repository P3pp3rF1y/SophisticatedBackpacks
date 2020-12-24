package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.util.IReorderingProcessor;
import net.minecraft.util.text.ITextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@OnlyIn(Dist.CLIENT)
public class ToggleButton<T extends Comparable<T>> extends Button {
	private final Map<T, StateData> stateData;
	private final Supplier<T> getState;

	public ToggleButton(Position position, ButtonDefinition.Toggle<T> buttonDefinition, Consumer<Integer> onClick, Supplier<T> getState) {
		super(position, buttonDefinition, onClick);

		stateData = buttonDefinition.getStateData();
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

		public StateData(TextureBlitData texture, List<? extends ITextComponent> tooltip) {
			this.texture = texture;
			this.tooltip = tooltip.stream().map(ITextComponent::func_241878_f).collect(Collectors.toList());
		}

		public StateData(TextureBlitData texture, ITextComponent... tooltip) {
			this.texture = texture;
			this.tooltip = Arrays.stream(tooltip).map(ITextComponent::func_241878_f).collect(Collectors.toList());
		}

		public TextureBlitData getTexture() {
			return texture;
		}

		public List<IReorderingProcessor> getTooltip() {
			return tooltip;
		}
	}
}
