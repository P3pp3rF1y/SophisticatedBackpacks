package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec2;
import net.minecraft.world.phys.Vec3;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.common.util.TriState;
import net.neoforged.neoforge.event.entity.EntityLeaveLevelEvent;
import net.neoforged.neoforge.event.entity.EntityMobGriefingEvent;
import net.neoforged.neoforge.event.entity.living.FinalizeSpawnEvent;
import net.neoforged.neoforge.event.entity.living.LivingConversionEvent;
import net.neoforged.neoforge.event.entity.living.LivingDropsEvent;
import net.neoforged.neoforge.event.entity.player.AttackEntityEvent;
import net.neoforged.neoforge.event.entity.player.ItemEntityPickupEvent;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.entity.player.PlayerInteractEvent;
import net.neoforged.neoforge.event.level.BlockEvent;
import net.neoforged.neoforge.event.tick.LevelTickEvent;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IAttackEntityResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.UUIDDeduplicator;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModPayloads;
import net.p3pp3rf1y.sophisticatedbackpacks.network.AnotherPlayerBackpackOpenPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.network.SyncPlayerSettingsPayload;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsManager;
import net.p3pp3rf1y.sophisticatedcore.upgrades.infinity.InfinityUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

public class CommonEventHandler {
	public void registerHandlers(IEventBus modBus) {
		ModItems.registerHandlers(modBus);
		ModBlocks.registerHandlers(modBus);
		modBus.addListener(ModPayloads::registerPayloads);
		IEventBus eventBus = NeoForge.EVENT_BUS;
		eventBus.addListener(this::onItemPickup);
		eventBus.addListener(this::onLivingSpecialSpawn);
		eventBus.addListener(this::onLivingConversionPre);
		eventBus.addListener(this::onLivingConversion);
		eventBus.addListener(this::onLivingDrops);
		eventBus.addListener(this::onEntityMobGriefing);
		eventBus.addListener(this::onEntityLeaveWorld);
		eventBus.addListener(this::onBlockClick);
		eventBus.addListener(this::onAttackEntity);
		eventBus.addListener(EntityBackpackAdditionHandler::onLivingUpdate);
		eventBus.addListener(this::onPlayerChangedDimension);
		eventBus.addListener(this::onPlayerRespawn);
		eventBus.addListener(this::onWorldTick);
		eventBus.addListener(this::interactWithEntity);
		eventBus.addListener(this::handleBreakBackpackWithInfinityUpgrade);
	}

	private static final int BACKPACK_CHECK_COOLDOWN = 40;
	private final Map<ResourceLocation, Long> nextBackpackCheckTime = new HashMap<>();

	private void interactWithEntity(PlayerInteractEvent.EntityInteractSpecific event) {
		if (event.getTarget() instanceof LivingEntity livingEntity && event.getEntity().isShiftKeyDown()) {
			InteractionResult result = MobCatcherHandler.tryCapture(event.getEntity(), event.getHand(), livingEntity);
			if (result != InteractionResult.PASS) {
				event.setCancellationResult(result);
				event.setCanceled(true);
				return;
			}
		}

		if (!(event.getTarget() instanceof Player targetPlayer) || Boolean.FALSE.equals(Config.SERVER.allowOpeningOtherPlayerBackpacks.get())) {
			return;
		}

		Player sourcePlayer = event.getEntity();
		Vec3 targetPlayerViewVector = Vec3.directionFromRotation(new Vec2(targetPlayer.getXRot(), targetPlayer.yBodyRot));

		Vec3 hitVector = event.getLocalPos();
		Vec3 vec31 = sourcePlayer.position().vectorTo(targetPlayer.position()).normalize();
		vec31 = new Vec3(vec31.x, 0.0D, vec31.z);
		boolean isPointingAtBody = hitVector.y >= 0.9D && hitVector.y < 1.6D;
		boolean isPointingAtBack = vec31.dot(targetPlayerViewVector) > 0.0D;
		if (!isPointingAtBody || !isPointingAtBack) {
			return;
		}
		if (targetPlayer.level().isClientSide) {
			event.setCancellationResult(InteractionResult.SUCCESS);
			PacketDistributor.sendToServer(new AnotherPlayerBackpackOpenPayload(targetPlayer.getId()));
		}
	}

	private void onWorldTick(LevelTickEvent.Post event) {
		if (event.getLevel().isClientSide()) {
			return;
		}

		ResourceLocation dimensionKey = event.getLevel().dimension().location();
		boolean runSlownessLogic = Boolean.TRUE.equals(Config.SERVER.nerfsConfig.tooManyBackpacksSlowness.get());
		boolean runDedupeLogic = Boolean.FALSE.equals(Config.SERVER.tickDedupeLogicDisabled.get());
		if ((!runSlownessLogic && !runDedupeLogic)
				|| nextBackpackCheckTime.getOrDefault(dimensionKey, 0L) > event.getLevel().getGameTime()) {
			return;
		}
		nextBackpackCheckTime.put(dimensionKey, event.getLevel().getGameTime() + BACKPACK_CHECK_COOLDOWN);

		Map<UUID, IBackpackWrapper> backpackIds = new HashMap<>();

		event.getLevel().players().forEach(player -> {
			Set<ItemStack> allBackpacks = new HashSet<>();
			PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, handlerName, identifier, slot) -> {
				if (runSlownessLogic) {
					allBackpacks.add(backpack);
				}
				if (runDedupeLogic) {
					addBackpackIdIfUniqueOrDedupe(backpackIds, BackpackWrapper.fromStack(backpack));
				}
				return false;
			});
			if (runSlownessLogic) {
				int maxNumberOfBackpacks = Config.SERVER.nerfsConfig.maxNumberOfBackpacks.get();
				if (allBackpacks.size() > maxNumberOfBackpacks) {
					int numberOfSlownessLevels = Math.min(10, (int) Math.ceil((allBackpacks.size() - maxNumberOfBackpacks) * Config.SERVER.nerfsConfig.slownessLevelsPerAdditionalBackpack.get()));
					Holder<MobEffect> effect = Config.SERVER.nerfsConfig.getEffect();
					player.addEffect(new MobEffectInstance(effect, BACKPACK_CHECK_COOLDOWN * 2, numberOfSlownessLevels - 1, false, false));
				}
			}
		});
	}

	private static void addBackpackIdIfUniqueOrDedupe(Map<UUID, IBackpackWrapper> backpackIds, IBackpackWrapper backpackWrapper) {
		backpackWrapper.getContentsUuid().ifPresent(backpackId -> {
			IBackpackWrapper existingBackpackWrapper = backpackIds.get(backpackId);
			if (existingBackpackWrapper == null || existingBackpackWrapper.getBackpack() == backpackWrapper.getBackpack()) {
				backpackIds.put(backpackId, backpackWrapper);
				return;
			}

			IBackpackWrapper backpackToKeep = UUIDDeduplicator.dedupeBackpackWrappers(existingBackpackWrapper, backpackWrapper);
			backpackIds.put(backpackId, backpackToKeep);
		});
	}

	private void onPlayerChangedDimension(PlayerEvent.PlayerChangedDimensionEvent event) {
		sendPlayerSettingsToClient(event.getEntity());
	}

	private void sendPlayerSettingsToClient(Player player) {
		if (player instanceof ServerPlayer serverPlayer) {
			String playerTagName = BackpackMainSettingsCategory.SOPHISTICATED_BACKPACK_SETTINGS_PLAYER_TAG;
			PacketDistributor.sendToPlayer(serverPlayer, new SyncPlayerSettingsPayload(playerTagName, SettingsManager.getPlayerSettingsTag(player, playerTagName)));
		}
	}

	private void onPlayerRespawn(PlayerEvent.PlayerRespawnEvent event) {
		sendPlayerSettingsToClient(event.getEntity());
	}

	private void onBlockClick(PlayerInteractEvent.LeftClickBlock event) {
		if (event.getLevel().isClientSide) {
			return;
		}
		Player player = event.getEntity();
		BlockPos pos = event.getPos();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
			for (IBlockClickResponseUpgrade upgrade : wrapper.getUpgradeHandler().getWrappersThatImplement(IBlockClickResponseUpgrade.class)) {
				if (upgrade.onBlockClick(player, pos)) {
					return true;
				}
			}
			return false;
		});
	}

	private void onAttackEntity(AttackEntityEvent event) {
		Player player = event.getEntity();
		if (player.level().isClientSide) {
			return;
		}
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
			for (IAttackEntityResponseUpgrade upgrade : wrapper.getUpgradeHandler().getWrappersThatImplement(IAttackEntityResponseUpgrade.class)) {
				if (upgrade.onAttackEntity(player)) {
					return true;
				}
			}
			return false;
		});
	}

	private void onLivingSpecialSpawn(FinalizeSpawnEvent event) {
		Entity entity = event.getEntity();
		if (entity instanceof Monster monster && monster.getItemBySlot(EquipmentSlot.CHEST).isEmpty()) {
			EntityBackpackAdditionHandler.handleBackpackAdditionOnSpawn(monster, event.getLevel());
		}
	}

	private void onLivingDrops(LivingDropsEvent event) {
		EntityBackpackAdditionHandler.handleBackpackDrop(event);
	}

	private void onLivingConversionPre(LivingConversionEvent.Pre event) {
		EntityBackpackAdditionHandler.handleLivingConversionPre(event);
	}

	private void onLivingConversion(LivingConversionEvent.Post event) {
		EntityBackpackAdditionHandler.handleLivingConversion(event);
	}

	private void onEntityMobGriefing(EntityMobGriefingEvent event) {
		if (event.getEntity() instanceof Creeper creeper) {
			EntityBackpackAdditionHandler.removeBeneficialEffects(creeper);
		}
	}

	private void onEntityLeaveWorld(EntityLeaveLevelEvent event) {
		if (!(event.getEntity() instanceof Monster)) {
			return;
		}
		EntityBackpackAdditionHandler.removeBackpackUuid((Monster) event.getEntity(), event.getLevel());
	}

	private void onItemPickup(ItemEntityPickupEvent.Pre event) {
		ItemEntity itemEntity = event.getItemEntity();
		if (itemEntity.getItem().isEmpty() || itemEntity.pickupDelay > 0) {
			return;
		}

		AtomicReference<ItemStack> remainingStackSimulated = new AtomicReference<>(itemEntity.getItem().copy());
		Player player = event.getPlayer();
		Level level = player.getCommandSenderWorld();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> {
					IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
					remainingStackSimulated.set(InventoryHelper.runPickupOnPickupResponseUpgrades(level, wrapper.getUpgradeHandler(), remainingStackSimulated.get(), true));
					return remainingStackSimulated.get().isEmpty();
				}, Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get()
		);

		if (remainingStackSimulated.get().getCount() != itemEntity.getItem().getCount()) {
			AtomicReference<ItemStack> remainingStack = new AtomicReference<>(itemEntity.getItem().copy());
			PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> {
						IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
						remainingStack.set(InventoryHelper.runPickupOnPickupResponseUpgrades(level, player, wrapper.getUpgradeHandler(), remainingStack.get(), false));
						return remainingStack.get().isEmpty();
					}
					, Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get()
			);
			itemEntity.setItem(remainingStack.get());
			event.setCanPickup(TriState.FALSE); //cancelling even when the stack isn't empty at this point to prevent full stack from before pickup to be picked up by player
		}
	}

	private void handleBreakBackpackWithInfinityUpgrade(BlockEvent.BreakEvent event) {
		Player player = event.getPlayer();

		if (!(event.getState().getBlock() instanceof BackpackBlock)) {
			return;
		}

		if (WorldHelper.getBlockEntity(event.getLevel(), event.getPos(), BackpackBlockEntity.class)
				.map(backpackBlockEntity -> backpackBlockEntity.getStorageWrapper().getUpgradeHandler().getTypeWrappers(InfinityUpgradeItem.TYPE)
						.stream().anyMatch(w -> !player.hasPermissions(w.getPermissionLevel())))
				.orElse(false)) {
			event.setCanceled(true);
			player.displayClientMessage(SBPTranslationHelper.INSTANCE.translStatusMessage("infinity_upgrade_only_admin_break").withStyle(ChatFormatting.RED), true);
		}
	}
}
