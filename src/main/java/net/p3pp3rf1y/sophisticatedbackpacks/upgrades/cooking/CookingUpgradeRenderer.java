package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.particles.ParticleTypes;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderer;

import java.util.Random;
import java.util.function.UnaryOperator;

public class CookingUpgradeRenderer implements IUpgradeRenderer<CookingUpgradeRenderData> {
	@Override
	public void render(World world, Random rand, UnaryOperator<Vector3d> getPositionFromOffset, CookingUpgradeRenderData upgradeRenderData) {
		if (!upgradeRenderData.isBurning()) {
			return;
		}

		if (world.random.nextDouble() < 0.1D) {
			Vector3d backpackBack = getPositionFromOffset.apply(Vector3d.ZERO);
			world.playLocalSound(backpackBack.x, backpackBack.y, backpackBack.z, SoundEvents.FURNACE_FIRE_CRACKLE, SoundCategory.BLOCKS, 1.0F, 1.0F, false);
		}

		double xOffset = world.random.nextDouble() * 0.6D - 0.3D;
		double yOffset = world.random.nextDouble() * 6.0D / 16.0D;
		double zOffset = 0.02D;
		Vector3d randomAtTheBack = getPositionFromOffset.apply(new Vector3d(xOffset, yOffset, zOffset));

		world.addParticle(ParticleTypes.SMOKE, randomAtTheBack.x, randomAtTheBack.y, randomAtTheBack.z, 0.0D, 0.0D, 0.0D);
		world.addParticle(ParticleTypes.FLAME, randomAtTheBack.x, randomAtTheBack.y, randomAtTheBack.z, 0.0D, 0.0D, 0.0D);
	}

}
