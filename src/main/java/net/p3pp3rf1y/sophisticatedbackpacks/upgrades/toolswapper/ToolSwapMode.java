package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import com.google.common.collect.ImmutableMap;
import com.mojang.serialization.Codec;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.util.StringRepresentable;
import net.neoforged.neoforge.network.codec.NeoForgeStreamCodecs;

import java.util.Map;

public enum ToolSwapMode implements StringRepresentable {
	ANY("name"), ONLY_TOOLS("onlyTools"), NO_SWAP("noSwap");

	public static final Codec<ToolSwapMode> CODEC = StringRepresentable.fromEnum(ToolSwapMode::values);
	public static final StreamCodec<FriendlyByteBuf, ToolSwapMode> STREAM_CODEC = NeoForgeStreamCodecs.enumCodec(ToolSwapMode.class);

	private final String name;

	ToolSwapMode(String name) {
		this.name = name;
	}

	@Override
	public String getSerializedName() {
		return name;
	}

	public ToolSwapMode next() {
		return VALUES[(ordinal() + 1) % VALUES.length];
	}

	private static final ToolSwapMode[] VALUES;
	private static final Map<String, ToolSwapMode> NAME_VALUES;

	static {
		ImmutableMap.Builder<String, ToolSwapMode> builder = new ImmutableMap.Builder<>();
		for (ToolSwapMode value : values()) {
			builder.put(value.getSerializedName(), value);
		}
		NAME_VALUES = builder.build();
		VALUES = values();
	}

	public static ToolSwapMode fromName(String name) {
		return NAME_VALUES.getOrDefault(name, ANY);
	}
}
