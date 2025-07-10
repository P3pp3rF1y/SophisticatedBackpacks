package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.core.UUIDUtil;
import net.minecraft.resources.ResourceLocation;

import java.util.UUID;

public record AccessLogRecord(ResourceLocation backpackItemRegistryName, UUID backpackUuid, String playerName,
							  String backpackName, int clothColor, int trimColor, long accessTime, int columnsTaken) {
	public static final Codec<AccessLogRecord> CODEC = RecordCodecBuilder.create(instance -> instance.group(
			ResourceLocation.CODEC.fieldOf("backpackItemRegistryName").forGetter(AccessLogRecord::backpackItemRegistryName),
			UUIDUtil.CODEC.fieldOf("backpackUuid").forGetter(AccessLogRecord::backpackUuid),
			Codec.STRING.fieldOf("playerName").forGetter(AccessLogRecord::playerName),
			Codec.STRING.fieldOf("backpackName").forGetter(AccessLogRecord::backpackName),
			Codec.INT.fieldOf("clothColor").forGetter(AccessLogRecord::clothColor),
			Codec.INT.fieldOf("trimColor").forGetter(AccessLogRecord::trimColor),
			Codec.LONG.fieldOf("accessTime").forGetter(AccessLogRecord::accessTime),
			Codec.INT.fieldOf("columnsTaken").forGetter(AccessLogRecord::columnsTaken)
	).apply(instance, AccessLogRecord::new));
}
