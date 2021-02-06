package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.IStringSerializable;

import java.util.Map;

public enum RestockFilterType implements IStringSerializable {
	ALLOW("allow"),
	BLOCK("block"),
	BACKPACK("backpack");

	private final String name;

	RestockFilterType(String name) {
		this.name = name;
	}

	@Override
	public String getString() {
		return name;
	}

	public RestockFilterType next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, RestockFilterType> NAME_VALUES;
	private static final RestockFilterType[] VALUES;

	static {
		ImmutableMap.Builder<String, RestockFilterType> builder = new ImmutableMap.Builder<>();
		for (RestockFilterType value : values()) {
			builder.put(value.getString(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static RestockFilterType fromName(String name) {
		return NAME_VALUES.getOrDefault(name, BLOCK);
	}
}
