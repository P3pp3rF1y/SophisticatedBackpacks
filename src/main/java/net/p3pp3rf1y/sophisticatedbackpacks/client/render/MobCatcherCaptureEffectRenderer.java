package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.util.Mth;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.phys.Vec3;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.util.Easing;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class MobCatcherCaptureEffectRenderer {
	private static final int ANIMATION_DURATION_TICKS = 5;
	private static final float POP_VOLUME = 0.7F;
	private static final float POP_BASE_PITCH = 0.7F;
	private static final float POP_PITCH_VARIATION = 0.16F;
	private static final double PARTICLE_SPREAD = 0.25D;

	private static final List<CaptureEffect> CAPTURE_EFFECTS = new ArrayList<>();
	private static final Set<ResourceLocation> ENTITY_RENDER_FAILURES = new HashSet<>();

	private MobCatcherCaptureEffectRenderer() {
	}

	public static void addEffect(ResourceLocation entityTypeName, CompoundTag entityNbt, Vec3 position, Vec3 collapsePosition, float yRot, float xRot) {
		Minecraft mc = Minecraft.getInstance();
		ClientLevel level = mc.level;
		if (level == null) {
			return;
		}

		if (ENTITY_RENDER_FAILURES.contains(entityTypeName)) {
			addNonRenderingEffect(entityTypeName, collapsePosition, level.getGameTime());
			return;
		}

		try {
			Entity entity = BuiltInRegistries.ENTITY_TYPE.getOptional(entityTypeName).map(entityType -> entityType.create(level)).orElse(null);
			if (!(entity instanceof LivingEntity livingEntity)) {
				return;
			}

			livingEntity.load(entityNbt.copy());
			livingEntity.moveTo(position.x, position.y, position.z, yRot, xRot);
			livingEntity.xOld = position.x;
			livingEntity.yOld = position.y;
			livingEntity.zOld = position.z;
			livingEntity.yRotO = yRot;
			livingEntity.xRotO = xRot;
			pinYRotation(livingEntity, yRot);
			CAPTURE_EFFECTS.add(new CaptureEffect(entityTypeName, livingEntity, collapsePosition, livingEntity.getBbWidth(), livingEntity.getBbHeight(), yRot, level.getGameTime()));
		} catch (RuntimeException e) {
			logRenderFailure(entityTypeName, e);
			addNonRenderingEffect(entityTypeName, collapsePosition, level.getGameTime());
		}
	}

	public static void render(PoseStack poseStack, float partialTick, Vec3 cameraPos) {
		Minecraft mc = Minecraft.getInstance();
		ClientLevel level = mc.level;
		if (level == null || CAPTURE_EFFECTS.isEmpty()) {
			return;
		}

		EntityRenderDispatcher entityRenderDispatcher = mc.getEntityRenderDispatcher();
		MultiBufferSource.BufferSource buffer = mc.renderBuffers().bufferSource();
		Iterator<CaptureEffect> iterator = CAPTURE_EFFECTS.iterator();
		while (iterator.hasNext()) {
			CaptureEffect effect = iterator.next();
			float progress = Mth.clamp((level.getGameTime() - effect.startTime() + partialTick) / ANIMATION_DURATION_TICKS, 0F, 1F);
			if (progress >= 1F) {
				finishEffect(level, effect);
				iterator.remove();
				continue;
			}

			if (effect.shouldRender()) {
				renderEffect(effect, poseStack, partialTick, cameraPos, entityRenderDispatcher, buffer, progress);
			}
		}
	}

	public static void clear() {
		CAPTURE_EFFECTS.clear();
	}

	private static void addNonRenderingEffect(ResourceLocation entityTypeName, Vec3 collapsePosition, long startTime) {
		CAPTURE_EFFECTS.add(new CaptureEffect(entityTypeName, null, collapsePosition, 1F, 1F, 0F, startTime));
	}

	private static void renderEffect(CaptureEffect effect, PoseStack poseStack, float partialTick, Vec3 cameraPos, EntityRenderDispatcher entityRenderDispatcher,
			MultiBufferSource.BufferSource buffer, float progress) {
		LivingEntity entity = effect.entity();
		if (entity == null) {
			return;
		}

		float scale = 1F - Easing.EASE_IN_CUBIC.ease(progress);
		Vec3 collapsePosition = effect.collapsePosition();
		Vec3 entityPosition = entity.position();

		poseStack.pushPose();
		try {
			pinYRotation(entity, effect.yRot());
			poseStack.translate(collapsePosition.x() - cameraPos.x(), collapsePosition.y() - cameraPos.y(), collapsePosition.z() - cameraPos.z());
			poseStack.scale(scale, scale, scale);
			poseStack.translate(entityPosition.x() - collapsePosition.x(), entityPosition.y() - collapsePosition.y(), entityPosition.z() - collapsePosition.z());
			entityRenderDispatcher.setRenderShadow(false);
			entityRenderDispatcher.render(entity, 0D, 0D, 0D, effect.yRot(), partialTick, poseStack, buffer, entityRenderDispatcher.getPackedLightCoords(entity, partialTick));
		} catch (RuntimeException e) {
			logRenderFailure(effect.entityType(), e);
			effect.disableRendering();
		} finally {
			entityRenderDispatcher.setRenderShadow(true);
			poseStack.popPose();
		}
	}

	private static void finishEffect(ClientLevel level, CaptureEffect effect) {
		Vec3 center = effect.collapsePosition();
		RandomSource random = level.random;
		for (int i = 0; i < 10; i++) {
			level.addParticle(ParticleTypes.POOF, center.x + randomOffset(random, effect.width()), center.y + randomOffset(random, effect.height()),
					center.z + randomOffset(random, effect.width()), random.nextGaussian() * 0.03D, random.nextGaussian() * 0.03D, random.nextGaussian() * 0.03D);
		}
		for (int i = 0; i < 6; i++) {
			level.addParticle(ParticleTypes.REVERSE_PORTAL, center.x + randomOffset(random, effect.width()), center.y + randomOffset(random, effect.height()),
					center.z + randomOffset(random, effect.width()), random.nextGaussian() * 0.02D, random.nextGaussian() * 0.02D, random.nextGaussian() * 0.02D);
		}

		float pitch = POP_BASE_PITCH + (random.nextFloat() - 0.5F) * POP_PITCH_VARIATION;
		level.playLocalSound(center.x, center.y, center.z, SoundEvents.ITEM_PICKUP, SoundSource.PLAYERS, POP_VOLUME, pitch, false);
	}

	private static double randomOffset(RandomSource random, double size) {
		return (random.nextDouble() - 0.5D) * Math.max(PARTICLE_SPREAD, size * 0.5D);
	}

	private static void pinYRotation(LivingEntity entity, float yRot) {
		entity.setYRot(yRot);
		entity.yRotO = yRot;
		entity.yBodyRot = yRot;
		entity.yBodyRotO = yRot;
		entity.yHeadRot = yRot;
		entity.yHeadRotO = yRot;
	}

	private static void logRenderFailure(ResourceLocation entityTypeName, RuntimeException e) {
		if (ENTITY_RENDER_FAILURES.add(entityTypeName)) {
			SophisticatedBackpacks.LOGGER.warn("Unable to render mob catcher capture effect for entity type {}", entityTypeName, e);
		}
	}

	private static class CaptureEffect {
		private final ResourceLocation entityType;
		@Nullable
		private final LivingEntity entity;
		private final Vec3 collapsePosition;
		private final float width;
		private final float height;
		private final float yRot;
		private final long startTime;
		private boolean renderDisabled;

		private CaptureEffect(ResourceLocation entityType, @Nullable LivingEntity entity, Vec3 collapsePosition, float width, float height, float yRot, long startTime) {
			this.entityType = entityType;
			this.entity = entity;
			this.collapsePosition = collapsePosition;
			this.width = width;
			this.height = height;
			this.yRot = yRot;
			this.startTime = startTime;
		}

		private ResourceLocation entityType() {
			return entityType;
		}

		@Nullable
		private LivingEntity entity() {
			return entity;
		}

		private Vec3 collapsePosition() {
			return collapsePosition;
		}

		private float width() {
			return width;
		}

		private float height() {
			return height;
		}

		private float yRot() {
			return yRot;
		}

		private long startTime() {
			return startTime;
		}

		private boolean shouldRender() {
			return entity != null && !renderDisabled;
		}

		private void disableRendering() {
			renderDisabled = true;
		}
	}
}
