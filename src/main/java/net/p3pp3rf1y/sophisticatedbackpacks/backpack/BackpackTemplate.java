package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.resources.ResourceLocation;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

public record BackpackTemplate(ResourceLocation itemRegistryName, ContainerContents contents) {
	public static final Codec<BackpackTemplate> CODEC = RecordCodecBuilder
			.create(inst -> inst.group(ResourceLocation.CODEC.fieldOf("itemRegistryName").forGetter(BackpackTemplate::itemRegistryName),
					ContainerContents.CODEC.fieldOf("contents").forGetter(BackpackTemplate::contents)).apply(inst, BackpackTemplate::new));
}
