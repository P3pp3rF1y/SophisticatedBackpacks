package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.IStringSerializable;

import java.util.Map;

public enum DepositFilterType implements IStringSerializable {
	ALLOW("allow"),
	BLOCK("block"),
	INVENTORY("inventory");

	private final String name;

	DepositFilterType(String name) {
		this.name = name;
	}

	@Override
	public String getSerializedName() {
		return name;
	}

	public DepositFilterType next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, DepositFilterType> NAME_VALUES;
	private static final DepositFilterType[] VALUES;

	static {
		ImmutableMap.Builder<String, DepositFilterType> builder = new ImmutableMap.Builder<>();
		for (DepositFilterType value : values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static DepositFilterType fromName(String name) {
		return NAME_VALUES.getOrDefault(name, BLOCK);
	}

}
