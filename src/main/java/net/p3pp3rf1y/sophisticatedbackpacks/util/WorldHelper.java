package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.World;

import javax.annotation.Nullable;
import java.util.Optional;

public class WorldHelper {
	private WorldHelper() {}

	public static Optional<TileEntity> getTile(@Nullable IBlockReader world, BlockPos pos) {
		return getTile(world, pos, TileEntity.class);
	}

	public static <T> Optional<T> getTile(@Nullable IBlockReader world, BlockPos pos, Class<T> teClass) {
		if (world == null) {
			return Optional.empty();
		}

		TileEntity te = world.getBlockEntity(pos);

		if (teClass.isInstance(te)) {
			return Optional.of(teClass.cast(te));
		}

		return Optional.empty();
	}

	public static void notifyBlockUpdate(TileEntity tile) {
		World world = tile.getLevel();
		if (world == null) {
			return;
		}
		world.sendBlockUpdated(tile.getBlockPos(), tile.getBlockState(), tile.getBlockState(), 3);
	}
}
