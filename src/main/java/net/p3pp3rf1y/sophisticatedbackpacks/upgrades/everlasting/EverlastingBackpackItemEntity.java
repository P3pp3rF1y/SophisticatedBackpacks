package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting;

import net.minecraft.entity.EntityType;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.network.IPacket;
import net.minecraft.particles.ParticleTypes;
import net.minecraft.util.DamageSource;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.fml.network.NetworkHooks;

@SuppressWarnings("java:S2160") //no need to override equals, the default implementation is good
public class EverlastingBackpackItemEntity extends ItemEntity {
	private boolean wasFloatingUp = false;

	public EverlastingBackpackItemEntity(EntityType<? extends ItemEntity> type, World world) {
		super(type, world);
		lifespan = Integer.MAX_VALUE; //set to not despawn
	}

	@Override
	public void tick() {
		if (!level.isClientSide) {
			double d0 = getX() + 0.5F - random.nextFloat();
			double d1 = getY() + random.nextFloat() * 0.5F;
			double d2 = getZ() + 0.5F - random.nextFloat();
			ServerWorld serverWorld = (ServerWorld) level;
			if (random.nextInt(20) == 0) {
				serverWorld.sendParticles(ParticleTypes.HAPPY_VILLAGER, d0, d1, d2, 0, 0, 0.1D, 0, 1f);
			}
		}
		if (!isNoGravity()) {
			if (isInWater() || isInLava()) {
				onInsideBubbleColumn(false);
				wasFloatingUp = true;
			} else if (wasFloatingUp) {
				setNoGravity(true);
				setDeltaMovement(Vector3d.ZERO);
			}
		}
		super.tick();
	}

	@Override
	public boolean isInWater() {
		return getY() < 1 || super.isInWater();
	}

	@Override
	public boolean fireImmune() {
		return true;
	}

	@Override
	public boolean ignoreExplosion() {
		return true;
	}

	@Override
	public boolean isInvulnerableTo(DamageSource source) {
		return true;
	}

	@Override
	protected void outOfWorld() {
		//do nothing as the only thing that vanilla does here is remove entity from world, but it can't for this
	}

	@Override
	public IPacket<?> getAddEntityPacket() {
		return NetworkHooks.getEntitySpawningPacket(this);
	}
}
