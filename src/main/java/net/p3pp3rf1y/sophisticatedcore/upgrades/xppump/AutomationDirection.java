package net.p3pp3rf1y.sophisticatedcore.upgrades.xppump;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.StringRepresentable;

import java.util.Map;

public enum AutomationDirection implements StringRepresentable {
	INPUT("input"),
	OUTPUT("output"),
	OFF("off");

	private final String name;

	AutomationDirection(String name) {this.name = name;}

	@Override
	public String getSerializedName() {
		return name;
	}

	public AutomationDirection next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, AutomationDirection> NAME_VALUES;
	private static final AutomationDirection[] VALUES;

	static {
		ImmutableMap.Builder<String, AutomationDirection> builder = new ImmutableMap.Builder<>();
		for (AutomationDirection value : AutomationDirection.values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static AutomationDirection fromName(String name) {
		return NAME_VALUES.getOrDefault(name, INPUT);
	}
}
