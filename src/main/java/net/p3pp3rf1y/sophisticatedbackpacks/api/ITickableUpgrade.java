package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;

public interface ITickableUpgrade {
	void tick(@Nullable LivingEntity entity, Level world, BlockPos pos);
}
