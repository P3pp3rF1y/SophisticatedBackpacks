package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.client.particle.IAnimatedSprite;
import net.minecraft.client.particle.IParticleFactory;
import net.minecraft.client.particle.IParticleRenderType;
import net.minecraft.client.particle.Particle;
import net.minecraft.client.particle.SpriteTexturedParticle;
import net.minecraft.client.world.ClientWorld;
import net.minecraft.util.math.MathHelper;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.Nullable;

public class JukeboxUpgradeNoteParticle extends SpriteTexturedParticle {
	protected JukeboxUpgradeNoteParticle(ClientWorld level, double x, double y, double z) {
		super(level, x, y, z, 0.0D, 0.0D, 0.0D);
		xd *= 0.01F;
		yd *= 0.05F;
		zd *= 0.01F;
		yd += 0.01D;
		double color = level.getRandom().nextDouble();
		rCol = Math.max(0.0F, MathHelper.sin(((float) color + 0.0F) * ((float) Math.PI * 2F)) * 0.65F + 0.35F);
		gCol = Math.max(0.0F, MathHelper.sin(((float) color + 0.33333334F) * ((float) Math.PI * 2F)) * 0.65F + 0.35F);
		bCol = Math.max(0.0F, MathHelper.sin(((float) color + 0.6666667F) * ((float) Math.PI * 2F)) * 0.65F + 0.35F);
		quadSize *= 1.5F;
		lifetime = 20;
	}

	@Override
	public IParticleRenderType getRenderType() {
		return IParticleRenderType.PARTICLE_SHEET_OPAQUE;
	}

	@Override
	public float getQuadSize(float pScaleFactor) {
		return quadSize * MathHelper.clamp((age + pScaleFactor) / lifetime * 32.0F, 0.0F, 1.0F);
	}

	@Override
	public void tick() {
		xo = x;
		yo = y;
		zo = z;
		if (age++ >= lifetime) {
			remove();
		} else {
			move(xd, yd, zd);
			if (y == yo) {
				xd *= 1.1D;
				zd *= 1.1D;
			}
			if (onGround) {
				xd *= 0.7F;
				zd *= 0.7F;
			}
		}
	}

	@OnlyIn(Dist.CLIENT)
	public static class Factory implements IParticleFactory<JukeboxUpgradeNoteParticleData> {
		private final IAnimatedSprite spriteSet;

		public Factory(IAnimatedSprite spriteSet) {
			this.spriteSet = spriteSet;
		}

		@Nullable
		@Override
		public Particle createParticle(JukeboxUpgradeNoteParticleData type, ClientWorld level, double x, double y, double z, double pXSpeed, double pYSpeed, double pZSpeed) {
			JukeboxUpgradeNoteParticle particle = new JukeboxUpgradeNoteParticle(level, x, y, z);
			particle.pickSprite(spriteSet);
			return particle;
		}
	}
}
