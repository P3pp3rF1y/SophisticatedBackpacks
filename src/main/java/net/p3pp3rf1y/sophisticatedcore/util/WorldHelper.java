package net.p3pp3rf1y.sophisticatedcore.util;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;

import javax.annotation.Nullable;
import java.util.Optional;

public class WorldHelper {
	private WorldHelper() {}

	public static Optional<BlockEntity> getBlockEntity(@Nullable BlockGetter world, BlockPos pos) {
		return getBlockEntity(world, pos, BlockEntity.class);
	}

	public static <T> Optional<T> getBlockEntity(@Nullable BlockGetter world, BlockPos pos, Class<T> teClass) {
		if (world == null) {
			return Optional.empty();
		}

		BlockEntity te = world.getBlockEntity(pos);

		if (teClass.isInstance(te)) {
			return Optional.of(teClass.cast(te));
		}

		return Optional.empty();
	}

	public static void notifyBlockUpdate(BlockEntity tile) {
		Level world = tile.getLevel();
		if (world == null) {
			return;
		}
		world.sendBlockUpdated(tile.getBlockPos(), tile.getBlockState(), tile.getBlockState(), 3);
	}
}
