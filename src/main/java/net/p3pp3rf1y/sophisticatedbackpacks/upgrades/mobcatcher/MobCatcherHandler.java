package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.*;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.animal.WaterAnimal;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.network.MobCatcherCaptureEffectPayload;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public class MobCatcherHandler {
	private MobCatcherHandler() {
	}

	public static InteractionResult tryCapture(Player player, InteractionHand hand, LivingEntity entity) {
		ItemStack stack = player.getItemInHand(hand);
		if (!(stack.getItem() instanceof BackpackItem)) {
			return InteractionResult.PASS;
		}

		IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
		Optional<MobCatcherUpgradeWrapper> upgradeWrapper = getBestUpgrade(backpackWrapper);
		if (upgradeWrapper.isEmpty()) {
			return InteractionResult.PASS;
		}

		if (player.level().isClientSide) {
			return InteractionResult.SUCCESS;
		}

		CaptureResult result = capture((ServerPlayer) player, backpackWrapper, entity, upgradeWrapper.get().isAdvanced());
		player.displayClientMessage(result.message(), true);
		return result.success() ? InteractionResult.CONSUME : InteractionResult.FAIL;
	}

	private static CaptureResult capture(ServerPlayer player, IBackpackWrapper backpackWrapper, LivingEntity entity, boolean advanced) {
		Optional<Component> eligibilityError = getEligibilityError(player, entity, advanced);
		if (eligibilityError.isPresent()) {
			return fail(player, eligibilityError.get());
		}

		boolean hostile = isHostile(entity);
		int slotCost = getSlotCost(entity, hostile);
		int maxSlotCost = advanced ? Config.SERVER.mobCatcherUpgrade.advancedMaxSlotCost.get() : Config.SERVER.mobCatcherUpgrade.basicMaxSlotCost.get();
		if (slotCost > maxSlotCost) {
			return fail(player, Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_too_large", slotCost, maxSlotCost));
		}

		CapturedMobFootprint footprint = MobCatcherStorage.getFootprint(entity, slotCost);
		Optional<Integer> slot = MobCatcherStorage.findEmptyRectangle(backpackWrapper, footprint);
		if (slot.isEmpty()) {
			return fail(player, Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_no_space", footprint.width(), footprint.height()));
		}

		CompoundTag entityTag = new CompoundTag();
		entity.saveWithoutId(entityTag);
		ResourceLocation entityType = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
		CapturedMob capturedMob = new CapturedMob(UUID.randomUUID(), entityType, entityTag, slot.get(), footprint.width(), footprint.height(), slotCost, hostile, getCapturedMobDisplayName(entity),
			(int) Math.ceil(entity.getHealth()), (int) Math.ceil(getEffectiveMaxHealth(entity)));
		MobCatcherStorage.addCapturedMob(backpackWrapper, capturedMob);
		PacketDistributor.sendToPlayersNear(player.serverLevel(), null, entity.getX(), entity.getY(), entity.getZ(), 64,
				new MobCatcherCaptureEffectPayload(entityType, entityTag, entity.position(), getCaptureEffectCollapsePosition(player, entity), entity.getYRot(), entity.getXRot()));
		entity.discard();
		syncCapturedMobs(player, backpackWrapper);
		return new CaptureResult(true, Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_captured", capturedMob.displayName()));
	}

	private static Vec3 getCaptureEffectCollapsePosition(ServerPlayer player, LivingEntity entity) {
		Vec3 eyePosition = player.getEyePosition();
		Vec3 lookPosition = eyePosition.add(player.getLookAngle().scale(player.entityInteractionRange() + 1D));
		return entity.getBoundingBox().clip(eyePosition, lookPosition).orElse(entity.position().add(0D, entity.getBbHeight() * 0.5D, 0D));
	}

	private static Optional<Component> getEligibilityError(ServerPlayer player, LivingEntity entity, boolean advanced) {
		if (entity instanceof Player) {
			return Optional.of(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_players_blocked"));
		}
		if (entity instanceof EnderDragon || entity instanceof WitherBoss) {
			return Optional.of(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_boss_blocked"));
		}
		if (entity.isPassenger() || entity.isVehicle()) {
			return Optional.of(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_passengers_blocked"));
		}
		if (Config.SERVER.mobCatcherUpgrade.matchesEntityBlockList(entity.getType())) {
			return Optional.of(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_blocklisted"));
		}
		if (entity instanceof OwnableEntity ownable && ownable.getOwnerUUID() != null && !ownable.getOwnerUUID().equals(player.getUUID())) {
			return Optional.of(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_not_owner"));
		}
		if (Boolean.TRUE.equals(Config.SERVER.mobCatcherUpgrade.disallowInventoryEntities.get()) && entity instanceof net.minecraft.world.Container) {
			return Optional.of(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_inventory_blocked"));
		}
		if (!advanced && isHostile(entity)) {
			return Optional.of(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_hostile_needs_advanced"));
		}
		return Optional.empty();
	}

	public static void release(ServerPlayer player, UUID capturedMobId) {
		if (!(player.containerMenu instanceof BackpackContainer backpackContainer)) {
			return;
		}
		IBackpackWrapper backpackWrapper = backpackContainer.getBackpackContext().getBackpackWrapper(player);
		Optional<CapturedMob> capturedMob = MobCatcherStorage.getCapturedMob(backpackWrapper, capturedMobId);
		if (capturedMob.isEmpty()) {
			return;
		}
		Optional<Entity> entity = createEntity(player.serverLevel(), capturedMob.get());
		if (entity.isEmpty() || !(entity.get() instanceof LivingEntity livingEntity)) {
			player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_release_failed"), true);
			return;
		}

		Optional<Vec3> target = getReleasePosition(player, livingEntity);
		if (target.isEmpty()) {
			player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_no_release_space"), true);
			playMobCatcherSound(player, SoundEvents.NOTE_BLOCK_BASS.value(), 0.7F, 0.8F);
			return;
		}

		livingEntity.moveTo(target.get().x, target.get().y, target.get().z, player.getYRot(), 0);
		if (!player.serverLevel().addFreshEntity(livingEntity)) {
			player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_release_failed"), true);
			return;
		}
		MobCatcherStorage.removeCapturedMob(backpackWrapper, capturedMobId);
		syncCapturedMobs(player, backpackWrapper);
		player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_released", capturedMob.get().displayName()), true);
		playMobCatcherSound(player, SoundEvents.ITEM_PICKUP, 0.7F, 1.2F);
	}

	private static String getCapturedMobDisplayName(LivingEntity entity) {
		return entity.hasCustomName() ? entity.getCustomName().getString() : entity.getType().getDescription().getString();
	}

	private static Optional<Entity> createEntity(ServerLevel level, CapturedMob capturedMob) {
		Optional<EntityType<?>> entityType = MobCatcherStorage.getEntityType(capturedMob);
		if (entityType.isEmpty()) {
			return Optional.empty();
		}
		Entity entity = entityType.get().create(level);
		if (entity == null) {
			return Optional.empty();
		}
		entity.load(capturedMob.entityNbt());
		return Optional.of(entity);
	}

	private static Optional<Vec3> getReleasePosition(ServerPlayer player, LivingEntity entity) {
		Vec3 eyePosition = player.getEyePosition();
		Vec3 lookPosition = eyePosition.add(player.getLookAngle().scale(player.blockInteractionRange()));
		HitResult hitResult = player.level().clip(new ClipContext(eyePosition, lookPosition, ClipContext.Block.COLLIDER, ClipContext.Fluid.NONE, player));
		if (hitResult instanceof BlockHitResult blockHitResult && hitResult.getType() != HitResult.Type.MISS) {
			BlockPos groundPos = blockHitResult.getBlockPos();
			Optional<Vec3> blockTarget = getValidReleasePosition(player, entity, groundPos.relative(blockHitResult.getDirection()))
					.or(() -> getValidReleasePosition(player, entity, groundPos.above()));
			if (blockTarget.isPresent()) {
				return blockTarget;
			}
		}

		Vec3 lookDirection = player.getLookAngle();
		Vec3 horizontalLookDirection = new Vec3(lookDirection.x, 0, lookDirection.z);
		if (horizontalLookDirection.lengthSqr() < 1.0E-4D) {
			horizontalLookDirection = Vec3.directionFromRotation(0, player.getYRot());
		}
		Vec3 normalizedLookDirection = horizontalLookDirection.normalize();
		for (int distance = 1; distance <= 2; distance++) {
			Vec3 candidatePosition = player.position().add(normalizedLookDirection.scale(distance));
			Optional<Vec3> fallbackTarget = getValidReleasePosition(player, entity, BlockPos.containing(candidatePosition.x, player.getY(), candidatePosition.z));
			if (fallbackTarget.isPresent()) {
				return fallbackTarget;
			}
		}
		return Optional.empty();
	}

	private static Optional<Vec3> getValidReleasePosition(ServerPlayer player, LivingEntity entity, BlockPos spawnPos) {
		Optional<Vec3> releasePosition = getReleasePositionOnGround(player, entity, spawnPos);
		if (releasePosition.isEmpty()) {
			return Optional.empty();
		}

		Vec3 pos = releasePosition.get();
		AABB bounds = entity.getDimensions(entity.getPose()).makeBoundingBox(pos);
		if (!player.level().noCollision(entity, bounds)) {
			return Optional.empty();
		}
		return Optional.of(pos);
	}

	private static Optional<Vec3> getReleasePositionOnGround(ServerPlayer player, LivingEntity entity, BlockPos spawnPos) {
		if (canReleaseWithoutGround(entity)) {
			return Optional.of(Vec3.atBottomCenterOf(spawnPos));
		}

		BlockPos groundPos = spawnPos.below();
		BlockState groundState = player.level().getBlockState(groundPos);
		VoxelShape collisionShape = groundState.getCollisionShape(player.level(), groundPos);
		if (collisionShape.isEmpty()) {
			return Optional.empty();
		}
		return Optional.of(new Vec3(spawnPos.getX() + 0.5D, groundPos.getY() + collisionShape.max(Direction.Axis.Y), spawnPos.getZ() + 0.5D));
	}

	private static boolean canReleaseWithoutGround(LivingEntity entity) {
		return entity instanceof FlyingMob || entity instanceof WaterAnimal || entity.isNoGravity();
	}

	public static Optional<MobCatcherUpgradeWrapper> getBestUpgrade(IBackpackWrapper backpackWrapper) {
		List<MobCatcherUpgradeWrapper> wrappers = backpackWrapper.getUpgradeHandler().getTypeWrappers(MobCatcherUpgradeItem.TYPE);
		return wrappers.stream().filter(MobCatcherUpgradeWrapper::isAdvanced).findFirst().or(() -> wrappers.stream().findFirst());
	}

	public static boolean isHostile(LivingEntity entity) {
		if (Config.SERVER.mobCatcherUpgrade.matchesPassiveOverrides(entity.getType())) {
			return false;
		}
		return Config.SERVER.mobCatcherUpgrade.matchesHostileOverrides(entity.getType()) || entity instanceof Enemy || entity.getType().getCategory() == MobCategory.MONSTER;
	}

	public static int getSlotCost(LivingEntity entity, boolean hostile) {
		double maxHealth = getEffectiveMaxHealth(entity);
		double currentHealth = Math.max(0D, entity.getHealth());
		double baseCost = maxHealth / 2D + Math.min(currentHealth, maxHealth) / 2D;
		double multiplier = hostile ? Config.SERVER.mobCatcherUpgrade.hostileMultiplier.get() : Config.SERVER.mobCatcherUpgrade.animalMultiplier.get();
		return Math.max(1, (int) Math.ceil(baseCost * multiplier));
	}

	static double getEffectiveMaxHealth(LivingEntity entity) {
		return Math.max(1D, Math.max(Math.max(entity.getAttributeValue(Attributes.MAX_HEALTH), entity.getMaxHealth()), entity.getHealth()));
	}

	private static CaptureResult fail(ServerPlayer player, Component message) {
		playMobCatcherSound(player, SoundEvents.NOTE_BLOCK_BASS.value(), 0.7F, 0.8F);
		return new CaptureResult(false, message);
	}

	private static void playMobCatcherSound(ServerPlayer player, SoundEvent sound, float volume, float basePitch) {
		float pitch = basePitch + (player.getRandom().nextFloat() - 0.5F) * 0.16F;
		player.level().playSound(null, player.blockPosition(), sound, SoundSource.PLAYERS, volume, pitch);
	}

	public static void syncCapturedMobs(ServerPlayer player, IBackpackWrapper backpackWrapper) {
		backpackWrapper.getContentsUuid().ifPresent(uuid -> {
			PacketDistributor.sendToPlayer(player, new BackpackContentsPayload(uuid, MobCatcherStorage.getCapturedMobsTag(backpackWrapper)));
			for (ServerPlayer viewer : player.server.getPlayerList().getPlayers()) {
				if (viewer.containerMenu instanceof BackpackContainer backpackContainer && isSameBackpack(backpackContainer, viewer, uuid)) {
					backpackContainer.syncClientStorageContentsToClient();
				}
			}
		});
	}

	private static boolean isSameBackpack(BackpackContainer backpackContainer, ServerPlayer player, UUID contentsUuid) {
		return backpackContainer.getBackpackContext().getBackpackWrapper(player).getContentsUuid().filter(contentsUuid::equals).isPresent();
	}

	private record CaptureResult(boolean success, Component message) {
	}
}
