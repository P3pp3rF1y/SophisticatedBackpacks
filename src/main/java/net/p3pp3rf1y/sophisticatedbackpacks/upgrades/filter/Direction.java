package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.IStringSerializable;

import java.util.Map;

public enum Direction implements IStringSerializable {
	BOTH("both"),
	INPUT("input"),
	OUTPUT("output");

	private final String name;

	Direction(String name) {
		this.name = name;
	}

	@Override
	public String getSerializedName() {
		return name;
	}

	public Direction next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, Direction> NAME_VALUES;
	private static final Direction[] VALUES;

	static {
		ImmutableMap.Builder<String, Direction> builder = new ImmutableMap.Builder<>();
		for (Direction value : Direction.values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static Direction fromName(String name) {
		return NAME_VALUES.getOrDefault(name, BOTH);
	}
}
