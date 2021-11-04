package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import com.mojang.math.Vector3f;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderer;

import java.util.Random;
import java.util.function.UnaryOperator;

public class SmeltingUpgradeRenderer implements IUpgradeRenderer<SmeltingUpgradeRenderData> {
	@Override
	public void render(Level level, Random rand, UnaryOperator<Vector3f> getPositionFromOffset, SmeltingUpgradeRenderData upgradeRenderData) {
		if (!upgradeRenderData.isBurning()) {
			return;
		}

		if (level.random.nextDouble() < 0.1D) {
			Vector3f backpackBack = getPositionFromOffset.apply(Vector3f.ZERO);
			level.playLocalSound(backpackBack.x(), backpackBack.y(), backpackBack.z(), SoundEvents.FURNACE_FIRE_CRACKLE, SoundSource.BLOCKS, 1.0F, 1.0F, false);
		}

		float xOffset = level.random.nextFloat() * 0.6f - 0.3f;
		float yOffset = level.random.nextFloat() * 6.0f / 16.0f;
		float zOffset = 0.02f;
		Vector3f randomAtTheBack = getPositionFromOffset.apply(new Vector3f(xOffset, yOffset, zOffset));

		level.addParticle(ParticleTypes.SMOKE, randomAtTheBack.x(), randomAtTheBack.y(), randomAtTheBack.z(), 0.0D, 0.0D, 0.0D);
		level.addParticle(ParticleTypes.FLAME, randomAtTheBack.x(), randomAtTheBack.y(), randomAtTheBack.z(), 0.0D, 0.0D, 0.0D);
	}
}
