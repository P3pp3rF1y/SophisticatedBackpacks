package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.entity.LivingEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import javax.annotation.Nullable;

public interface ITickableUpgrade {
	void tick(@Nullable LivingEntity entity, World world, BlockPos pos);
}
