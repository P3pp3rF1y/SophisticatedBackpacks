package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.google.common.collect.ImmutableMap;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import java.util.function.Predicate;
import java.util.function.Supplier;

public class BooleanToggleButton extends ToggleButton<Boolean> {
	public BooleanToggleButton(Position position, Dimension dimension, Predicate<Integer> onClick, TextureBlitData backgroundTexture, ToggleButton.StateData onStateData,
			ToggleButton.StateData offStateData, Supplier<Boolean> getState) {
		super(position, dimension, onClick, backgroundTexture, ImmutableMap.of(
				true, onStateData,
				false, offStateData
		), getState);
	}
}
