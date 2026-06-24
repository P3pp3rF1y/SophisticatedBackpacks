package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.client.color.item.ItemTintSource;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.util.ARGB;
import net.minecraft.util.ExtraCodecs;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.client.event.RegisterColorHandlersEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;

import javax.annotation.Nullable;

public class BackpackTintSources {
	public static void register(RegisterColorHandlersEvent.ItemTintSources event) {
		event.register(SophisticatedBackpacks.getRL("main"), Main.MAP_CODEC);
		event.register(SophisticatedBackpacks.getRL("accent"), Accent.MAP_CODEC);
	}

	public record Main(int defaultColor) implements ItemTintSource {
		public static final MapCodec<Main> MAP_CODEC = RecordCodecBuilder
				.mapCodec(instance -> instance.group(ExtraCodecs.RGB_COLOR_CODEC.fieldOf("default").forGetter(Main::defaultColor)).apply(instance, Main::new));

		public Main(int defaultColor) {
			this.defaultColor = ARGB.opaque(defaultColor);
		}

		@Override
		public int calculate(ItemStack itemStack, @Nullable ClientLevel clientLevel, @Nullable LivingEntity livingEntity) {
			return BackpackWrapper.fromStack(itemStack).getMainColor();
		}

		@Override
		public MapCodec<? extends ItemTintSource> type() {
			return MAP_CODEC;
		}
	}

	public record Accent(int defaultColor) implements ItemTintSource {
		public static final MapCodec<Accent> MAP_CODEC = RecordCodecBuilder.mapCodec(
				instance -> instance.group(ExtraCodecs.RGB_COLOR_CODEC.fieldOf("default").forGetter(Accent::defaultColor)).apply(instance, Accent::new));

		public Accent(int defaultColor) {
			this.defaultColor = ARGB.opaque(defaultColor);
		}

		@Override
		public int calculate(ItemStack itemStack, @Nullable ClientLevel clientLevel, @Nullable LivingEntity livingEntity) {
			return BackpackWrapper.fromStack(itemStack).getAccentColor();
		}

		@Override
		public MapCodec<? extends ItemTintSource> type() {
			return MAP_CODEC;
		}
	}
}
