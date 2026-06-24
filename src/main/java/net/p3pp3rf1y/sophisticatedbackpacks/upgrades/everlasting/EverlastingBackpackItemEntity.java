package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting;

import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

@SuppressWarnings("java:S2160") // no need to override equals, the default implementation is good
public class EverlastingBackpackItemEntity extends ItemEntity {
	private boolean wasFloatingUp = false;

	public EverlastingBackpackItemEntity(EntityType<? extends ItemEntity> type, Level level) {
		super(type, level);
		lifespan = Integer.MAX_VALUE; // set to not despawn
	}

	@Override
	public void tick() {
		if (!level().isClientSide) {
			double d0 = getX() + 0.5F - random.nextFloat();
			double d1 = getY() + random.nextFloat() * 0.5F;
			double d2 = getZ() + 0.5F - random.nextFloat();
			ServerLevel serverLevel = (ServerLevel) level();
			if (random.nextInt(20) == 0) {
				serverLevel.sendParticles(ParticleTypes.HAPPY_VILLAGER, d0, d1, d2, 0, 0, 0.1D, 0, 1f);
			}
		}
		if (!isNoGravity()) {
			if (isInWater() || isInLava()) {
				onInsideBubbleColumn(false);
				wasFloatingUp = true;
			} else if (wasFloatingUp) {
				setNoGravity(true);
				setDeltaMovement(Vec3.ZERO);
			}
		}
		super.tick();
	}

	@Override
	public boolean isInWater() {
		return getY() < level().getMinY() + 1 || super.isInWater();
	}

	@Override
	public boolean fireImmune() {
		return true;
	}

	@Override
	public boolean ignoreExplosion(Explosion explosion) {
		return true;
	}

	@Override
	protected void onBelowWorld() {
		// do nothing as the only thing that vanilla does here is remove entity from world, but it can't for this
	}
}
