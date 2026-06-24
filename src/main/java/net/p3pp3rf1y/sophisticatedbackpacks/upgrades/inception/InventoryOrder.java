package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import com.google.common.collect.ImmutableMap;
import com.mojang.serialization.Codec;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.util.StringRepresentable;
import net.neoforged.neoforge.network.codec.NeoForgeStreamCodecs;

import java.util.Map;

public enum InventoryOrder implements StringRepresentable {
	MAIN_FIRST("main_first"), INCEPTED_FIRST("incepted_first");

	public static final Codec<InventoryOrder> CODEC = StringRepresentable.fromEnum(InventoryOrder::values);
	public static final StreamCodec<FriendlyByteBuf, InventoryOrder> STREAM_CODEC = NeoForgeStreamCodecs.enumCodec(InventoryOrder.class);

	private final String name;

	InventoryOrder(String name) {
		this.name = name;
	}

	@Override
	public String getSerializedName() {
		return name;
	}

	public InventoryOrder next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final Map<String, InventoryOrder> NAME_VALUES;
	private static final InventoryOrder[] VALUES;

	static {
		ImmutableMap.Builder<String, InventoryOrder> builder = new ImmutableMap.Builder<>();
		for (InventoryOrder value : values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static InventoryOrder fromName(String name) {
		return NAME_VALUES.getOrDefault(name, MAIN_FIRST);
	}
}
