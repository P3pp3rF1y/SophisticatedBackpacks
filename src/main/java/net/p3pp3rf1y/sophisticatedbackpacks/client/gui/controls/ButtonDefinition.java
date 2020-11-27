package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import javax.annotation.Nullable;
import java.util.Map;

public class ButtonDefinition {
	private final Dimension dimension;
	private final TextureBlitData backgroundTexture;
	@Nullable
	private final TextureBlitData hoveredBackgroundTexture;

	public ButtonDefinition(Dimension dimension, TextureBlitData backgroundTexture) {
		this(dimension, backgroundTexture, null);
	}

	public ButtonDefinition(Dimension dimension, TextureBlitData backgroundTexture, @Nullable TextureBlitData hoveredBackgroundTexture) {
		this.dimension = dimension;
		this.backgroundTexture = backgroundTexture;
		this.hoveredBackgroundTexture = hoveredBackgroundTexture;
	}

	public Dimension getDimension() {
		return dimension;
	}

	public TextureBlitData getBackgroundTexture() {
		return backgroundTexture;
	}

	@Nullable
	public TextureBlitData getHoveredBackgroundTexture() {
		return hoveredBackgroundTexture;
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
