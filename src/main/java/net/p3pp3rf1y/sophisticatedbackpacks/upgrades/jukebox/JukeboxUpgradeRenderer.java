package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModParticles;

import java.util.Random;
import java.util.function.UnaryOperator;

public class JukeboxUpgradeRenderer implements IUpgradeRenderer<JukeboxUpgradeRenderData> {
	@Override
	public void render(World world, Random rand, UnaryOperator<Vector3d> getPositionFromOffset, JukeboxUpgradeRenderData upgradeRenderData) {
		if (!upgradeRenderData.isPlaying() || rand.nextInt(3) != 0) {
			return;
		}

		double xOffset = world.random.nextDouble() * 0.6D - 0.3D;
		double yOffset = 0.5 + world.random.nextDouble() * 6.0D / 16.0D;
		double zOffset = world.random.nextDouble() * 0.6D - 0.1D;
		Vector3d randomAtTheBack = getPositionFromOffset.apply(new Vector3d(xOffset, yOffset, zOffset));

		world.addParticle(ModParticles.JUKEBOX_NOTE.get(), randomAtTheBack.x, randomAtTheBack.y, randomAtTheBack.z, rand.nextFloat(), 0.0D, 0.0D);
	}
}
