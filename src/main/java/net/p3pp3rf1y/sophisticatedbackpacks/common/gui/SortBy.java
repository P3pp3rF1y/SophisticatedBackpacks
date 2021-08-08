package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.IStringSerializable;

import java.util.Map;

public enum SortBy implements IStringSerializable {
	NAME("name"),
	COUNT("count"),
	TAGS("tags");

	private final String name;

	SortBy(String name) {
		this.name = name;
	}

	@Override
	public String getSerializedName() {
		return name;
	}

	public SortBy next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, SortBy> NAME_VALUES;
	private static final SortBy[] VALUES;

	static {
		ImmutableMap.Builder<String, SortBy> builder = new ImmutableMap.Builder<>();
		for (SortBy value : SortBy.values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static SortBy fromName(String name) {
		return NAME_VALUES.getOrDefault(name, NAME);
	}
}
