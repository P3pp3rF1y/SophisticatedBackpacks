package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.StringTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class ButtonDefinition {
	private final Dimension dimension;
	@Nullable
	private final TextureBlitData backgroundTexture;
	@Nullable
	private final TextureBlitData hoveredBackgroundTexture;
	@Nullable
	private final TextureBlitData foregroundTexture;
	private final List<ITextProperties> tooltip;

	public ButtonDefinition(Dimension dimension, @Nullable TextureBlitData backgroundTexture, @Nullable TextureBlitData hoveredBackgroundTexture) {
		this(dimension, backgroundTexture, hoveredBackgroundTexture, null, new StringTextComponent(""));
	}

	public ButtonDefinition(Dimension dimension, @Nullable TextureBlitData backgroundTexture,
			@Nullable TextureBlitData hoveredBackgroundTexture, @Nullable TextureBlitData foregroundTexture, ITextProperties... tooltip) {
		this.dimension = dimension;
		this.backgroundTexture = backgroundTexture;
		this.hoveredBackgroundTexture = hoveredBackgroundTexture;
		this.foregroundTexture = foregroundTexture;
		this.tooltip = Arrays.asList(tooltip);
	}

	public Dimension getDimension() {
		return dimension;
	}

	@Nullable
	public TextureBlitData getBackgroundTexture() {
		return backgroundTexture;
	}

	@Nullable
	public TextureBlitData getHoveredBackgroundTexture() {
		return hoveredBackgroundTexture;
	}

	@Nullable
	public TextureBlitData getForegroundTexture() {
		return foregroundTexture;
	}

	public List<ITextProperties> getTooltip() {
		return tooltip;
	}

	public static class Toggle<T extends Comparable<T>> extends ButtonDefinition {
		private final Map<T, ToggleButton.StateData> stateData;

		public Toggle(Dimension dimension, TextureBlitData backgroundTexture, Map<T, ToggleButton.StateData> stateData,
				@Nullable TextureBlitData hoveredBackgroundTexture) {
			super(dimension, backgroundTexture, hoveredBackgroundTexture);
			this.stateData = stateData;
		}

		public Map<T, ToggleButton.StateData> getStateData() {
			return stateData;
		}
	}
}
