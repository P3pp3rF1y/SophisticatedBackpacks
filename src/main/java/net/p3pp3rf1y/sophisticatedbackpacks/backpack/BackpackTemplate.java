package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.resources.Identifier;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

public record BackpackTemplate(Identifier itemRegistryName, ContainerContents contents) {
	public static final Codec<BackpackTemplate> CODEC = RecordCodecBuilder
			.create(inst -> inst.group(Identifier.CODEC.fieldOf("itemRegistryName").forGetter(BackpackTemplate::itemRegistryName),
					ContainerContents.CODEC.fieldOf("contents").forGetter(BackpackTemplate::contents)).apply(inst, BackpackTemplate::new));
}
