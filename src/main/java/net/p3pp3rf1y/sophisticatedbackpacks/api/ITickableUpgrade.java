package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;

import javax.annotation.Nullable;

public interface ITickableUpgrade {
	void tick(@Nullable PlayerEntity player, World world, BlockPos pos, IBackpackWrapper wrapper);
}
