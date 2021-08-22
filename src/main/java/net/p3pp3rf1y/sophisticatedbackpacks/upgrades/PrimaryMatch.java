package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.IStringSerializable;

import java.util.Map;

public enum PrimaryMatch implements IStringSerializable {
	ITEM("item"),
	MOD("mod"),
	TAGS("tags");

	private final String name;

	PrimaryMatch(String name) {
		this.name = name;
	}

	@Override
	public String getSerializedName() {
		return name;
	}

	public PrimaryMatch next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, PrimaryMatch> NAME_VALUES;
	private static final PrimaryMatch[] VALUES;

	static {
		ImmutableMap.Builder<String, PrimaryMatch> builder = new ImmutableMap.Builder<>();
		for (PrimaryMatch value : PrimaryMatch.values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static PrimaryMatch fromName(String name) {
		return NAME_VALUES.getOrDefault(name, ITEM);
	}
}
