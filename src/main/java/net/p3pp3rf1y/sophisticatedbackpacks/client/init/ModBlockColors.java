package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.minecraft.client.color.block.BlockTintSource;
import net.minecraft.client.renderer.block.BlockAndTintGetter;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.event.RegisterColorHandlersEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import java.util.List;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.*;

public class ModBlockColors {
	private ModBlockColors() {}

	public static void registerBlockColorHandlers(RegisterColorHandlersEvent.BlockTintSources event) {
		event.register(List.of(new MainColorTintSource(), new AccentColorTintSource()), BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
	}

	private abstract static class BackpackTintSource implements BlockTintSource {
		@Override
		public int color(BlockState state) {
			return getDefaultColor();
		}

		@Override
		public int colorInWorld(BlockState state, BlockAndTintGetter level, BlockPos pos) {
			return WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class)
					.map(this::getColor)
					.orElse(getDefaultColor());
		}

		protected abstract int getColor(BackpackBlockEntity backpackBlockEntity);

		protected abstract int getDefaultColor();
	}

	private static class MainColorTintSource extends BackpackTintSource {
		@Override
		protected int getColor(BackpackBlockEntity backpackBlockEntity) {
			return backpackBlockEntity.getBackpackWrapper().getMainColor();
		}

		@Override
		protected int getDefaultColor() {
			return BackpackWrapper.DEFAULT_MAIN_COLOR;
		}
	}

	private static class AccentColorTintSource extends BackpackTintSource {
		@Override
		protected int getColor(BackpackBlockEntity backpackBlockEntity) {
			return backpackBlockEntity.getBackpackWrapper().getAccentColor();
		}

		@Override
		protected int getDefaultColor() {
			return BackpackWrapper.DEFAULT_ACCENT_COLOR;
		}
	}
}
