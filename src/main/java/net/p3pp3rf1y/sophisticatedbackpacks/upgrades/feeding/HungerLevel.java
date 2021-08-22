package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.IStringSerializable;

import java.util.Map;

public enum HungerLevel implements IStringSerializable {
	ANY("any"),
	HALF("half"),
	FULL("full");

	private final String name;

	HungerLevel(String name) {this.name = name;}

	@Override
	public String getSerializedName() {
		return name;
	}

	public HungerLevel next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, HungerLevel> NAME_VALUES;
	private static final HungerLevel[] VALUES;

	static {
		ImmutableMap.Builder<String, HungerLevel> builder = new ImmutableMap.Builder<>();
		for (HungerLevel value : HungerLevel.values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static HungerLevel fromName(String name) {
		return NAME_VALUES.getOrDefault(name, HALF);
	}
}
