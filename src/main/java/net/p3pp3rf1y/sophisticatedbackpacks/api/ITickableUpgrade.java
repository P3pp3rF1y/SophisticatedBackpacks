package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;

public interface ITickableUpgrade {
	void tick(World world, BlockPos pos, IBackpackWrapper wrapper);
}
