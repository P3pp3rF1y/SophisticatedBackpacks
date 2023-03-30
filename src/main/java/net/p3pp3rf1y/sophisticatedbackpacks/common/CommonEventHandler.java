package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec2;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.event.entity.EntityMobGriefingEvent;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.EntityItemPickupEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IAttackEntityResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.network.AnotherPlayerBackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedcore.network.SyncPlayerSettingsMessage;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsManager;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.ServerStorageSoundHandler;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class CommonEventHandler {
	public void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		ModItems.registerHandlers(modBus);
		ModBlocks.registerHandlers(modBus);
		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(this::onItemPickup);
		eventBus.addListener(this::onLivingSpecialSpawn);
		eventBus.addListener(this::onLivingDrops);
		eventBus.addListener(this::onEntityMobGriefing);
		eventBus.addListener(this::onEntityLeaveWorld);
		eventBus.addListener(ServerStorageSoundHandler::tick);
		eventBus.addListener(this::onBlockClick);
		eventBus.addListener(this::onAttackEntity);
		eventBus.addListener(EntityBackpackAdditionHandler::onLivingUpdate);
		eventBus.addListener(this::onPlayerLoggedIn);
		eventBus.addListener(this::onPlayerChangedDimension);
		eventBus.addListener(this::onPlayerRespawn);
		eventBus.addListener(this::onWorldTick);
		eventBus.addListener(this::interactWithEntity);
	}

	private static final int BACKPACK_CHECK_COOLDOWN = 40;
	private final Map<ResourceLocation, Long> nextBackpackCheckTime = new HashMap<>();

	private void interactWithEntity(PlayerInteractEvent.EntityInteractSpecific event) {
		if (!(event.getTarget() instanceof Player targetPlayer) || Boolean.FALSE.equals(Config.SERVER.allowOpeningOtherPlayerBackpacks.get())) {
			return;
		}

		Player sourcePlayer = event.getPlayer();
		Vec3 targetPlayerViewVector = Vec3.directionFromRotation(new Vec2(targetPlayer.getXRot(), targetPlayer.yBodyRot));

		Vec3 hitVector = event.getLocalPos();
		Vec3 vec31 = sourcePlayer.position().vectorTo(targetPlayer.position()).normalize();
		vec31 = new Vec3(vec31.x, 0.0D, vec31.z);
		boolean isPointingAtBody = hitVector.y >= 0.9D && hitVector.y < 1.6D;
		boolean isPointingAtBack = vec31.dot(targetPlayerViewVector) > 0.0D;
		if (!isPointingAtBody || !isPointingAtBack) {
			return;
		}
		if (targetPlayer.level.isClientSide) {
			event.setCancellationResult(InteractionResult.SUCCESS);
			SBPPacketHandler.INSTANCE.sendToServer(new AnotherPlayerBackpackOpenMessage(targetPlayer.getId()));
		}
	}

	private void onWorldTick(TickEvent.WorldTickEvent event) {
		ResourceLocation dimensionKey = event.world.dimension().location();
		boolean runSlownessLogic = Boolean.TRUE.equals(Config.SERVER.nerfsConfig.tooManyBackpacksSlowness.get());
		boolean runDedupeLogic = Boolean.FALSE.equals(Config.SERVER.tickDedupeLogicDisabled.get());
		if (event.phase != TickEvent.Phase.END
				|| (!runSlownessLogic && !runDedupeLogic)
				|| nextBackpackCheckTime.getOrDefault(dimensionKey, 0L) > event.world.getGameTime()) {
			return;
		}
		nextBackpackCheckTime.put(dimensionKey, event.world.getGameTime() + BACKPACK_CHECK_COOLDOWN);

		Set<UUID> backpackIds = new HashSet<>();

		event.world.players().forEach(player -> {
			AtomicInteger numberOfBackpacks = new AtomicInteger(0);
			PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, handlerName, identifier, slot) -> {
				if (runSlownessLogic) {
					numberOfBackpacks.incrementAndGet();
				}
				if (runDedupeLogic) {
					backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper ->
							addBackpackIdIfUniqueOrDedupe(backpackIds, backpackWrapper));
				}
				return false;
			});
			if (runSlownessLogic) {
				int maxNumberOfBackpacks = Config.SERVER.nerfsConfig.maxNumberOfBackpacks.get();
				if (numberOfBackpacks.get() > maxNumberOfBackpacks) {
					int numberOfSlownessLevels = Math.min(10, (int) Math.ceil((numberOfBackpacks.get() - maxNumberOfBackpacks) * Config.SERVER.nerfsConfig.slownessLevelsPerAdditionalBackpack.get()));
					player.addEffect(new MobEffectInstance(MobEffects.MOVEMENT_SLOWDOWN, BACKPACK_CHECK_COOLDOWN * 2, numberOfSlownessLevels - 1, false, false));
				}
			}
		});
	}

	private static void addBackpackIdIfUniqueOrDedupe(Set<UUID> backpackIds, IBackpackWrapper backpackWrapper) {
		backpackWrapper.getContentsUuid().ifPresent(backpackId -> {
			if (backpackIds.contains(backpackId)) {
				backpackWrapper.removeContentsUUIDTag();
				backpackWrapper.onContentsNbtUpdated();
			} else {
				backpackIds.add(backpackId);
			}
		});
	}

	private void onPlayerChangedDimension(PlayerEvent.PlayerChangedDimensionEvent event) {
		sendPlayerSettingsToClient(event.getPlayer());
	}

	private void onPlayerLoggedIn(PlayerEvent.PlayerLoggedInEvent event) {
		Player player = event.getPlayer();
		sendPlayerSettingsToClient(player);
	}

	private void sendPlayerSettingsToClient(Player player) {
		String playerTagName = BackpackMainSettingsCategory.SOPHISTICATED_BACKPACK_SETTINGS_PLAYER_TAG;
		PacketHandler.INSTANCE.sendToClient((ServerPlayer) player, new SyncPlayerSettingsMessage(playerTagName, SettingsManager.getPlayerSettingsTag(player, playerTagName)));
	}

	private void onPlayerRespawn(PlayerEvent.PlayerRespawnEvent event) {
		sendPlayerSettingsToClient(event.getPlayer());
	}

	private void onBlockClick(PlayerInteractEvent.LeftClickBlock event) {
		if (event.getWorld().isClientSide) {
			return;
		}
		Player player = event.getPlayer();
		BlockPos pos = event.getPos();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> {
					for (IBlockClickResponseUpgrade upgrade : wrapper.getUpgradeHandler().getWrappersThatImplement(IBlockClickResponseUpgrade.class)) {
						if (upgrade.onBlockClick(player, pos)) {
							return true;
						}
					}
					return false;
				}).orElse(false));
	}

	private void onAttackEntity(AttackEntityEvent event) {
		Player player = event.getPlayer();
		if (player.level.isClientSide) {
			return;
		}
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> {
					for (IAttackEntityResponseUpgrade upgrade : wrapper.getUpgradeHandler().getWrappersThatImplement(IAttackEntityResponseUpgrade.class)) {
						if (upgrade.onAttackEntity(player)) {
							return true;
						}
					}
					return false;
				}).orElse(false));
	}

	private void onLivingSpecialSpawn(LivingSpawnEvent.SpecialSpawn event) {
		Entity entity = event.getEntity();
		if (entity instanceof Monster monster && monster.getItemBySlot(EquipmentSlot.CHEST).isEmpty()) {
			EntityBackpackAdditionHandler.addBackpack(monster);
		}
	}

	private void onLivingDrops(LivingDropsEvent event) {
		EntityBackpackAdditionHandler.handleBackpackDrop(event);
	}

	private void onEntityMobGriefing(EntityMobGriefingEvent event) {
		if (event.getEntity() instanceof Creeper creeper) {
			EntityBackpackAdditionHandler.removeBeneficialEffects(creeper);
		}
	}

	private void onEntityLeaveWorld(EntityLeaveWorldEvent event) {
		if (!(event.getEntity() instanceof Monster)) {
			return;
		}
		EntityBackpackAdditionHandler.removeBackpackUuid((Monster) event.getEntity());
	}

	private void onItemPickup(EntityItemPickupEvent event) {
		ItemEntity itemEntity = event.getItem();
		if (itemEntity.getItem().isEmpty()) {
			return;
		}

		AtomicReference<ItemStack> remainingStackSimulated = new AtomicReference<>(itemEntity.getItem().copy());
		Player player = event.getPlayer();
		Level world = player.getCommandSenderWorld();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> {
					remainingStackSimulated.set(InventoryHelper.runPickupOnPickupResponseUpgrades(world, wrapper.getUpgradeHandler(), remainingStackSimulated.get(), true));
					return remainingStackSimulated.get().isEmpty();
				}).orElse(false), Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get()
		);

		if (remainingStackSimulated.get().getCount() != itemEntity.getItem().getCount()) {
			AtomicReference<ItemStack> remainingStack = new AtomicReference<>(itemEntity.getItem().copy());
			PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
							.map(wrapper -> {
								remainingStack.set(InventoryHelper.runPickupOnPickupResponseUpgrades(world, player, wrapper.getUpgradeHandler(), remainingStack.get(), false));
								return remainingStack.get().isEmpty();
							}).orElse(false)
					, Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get()
			);
			itemEntity.setItem(remainingStack.get());
			event.setCanceled(true); //cancelling even when the stack isn't empty at this point to prevent full stack from before pickup to be picked up by player
		}
	}
}
