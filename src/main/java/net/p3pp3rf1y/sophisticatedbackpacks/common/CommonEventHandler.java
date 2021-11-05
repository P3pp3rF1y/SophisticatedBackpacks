package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.MinecraftForge;
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
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IAttackEntityResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModFluids;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModParticles;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncPlayerSettingsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.ServerBackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import java.util.Random;

public class CommonEventHandler {
	public void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		ModItems.registerHandlers(modBus);
		ModBlocks.registerHandlers(modBus);
		ModFluids.registerHandlers(modBus);
		ModParticles.registerParticles(modBus);
		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(this::onItemPickup);
		eventBus.addListener(this::onLivingSpecialSpawn);
		eventBus.addListener(this::onLivingDrops);
		eventBus.addListener(this::onEntityMobGriefing);
		eventBus.addListener(this::onEntityLeaveWorld);
		eventBus.addListener(ServerBackpackSoundHandler::tick);
		eventBus.addListener(this::onBlockClick);
		eventBus.addListener(this::onAttackEntity);
		eventBus.addListener(EntityBackpackAdditionHandler::onLivingUpdate);
		eventBus.addListener(this::onPlayerLoggedIn);
		eventBus.addListener(this::onPlayerChangedDimension);
	}

	private void onPlayerChangedDimension(PlayerEvent.PlayerChangedDimensionEvent event) {
		PacketHandler.sendToClient((ServerPlayer) event.getPlayer(), new SyncPlayerSettingsMessage(BackpackSettingsManager.getPlayerBackpackSettingsTag(event.getPlayer())));
	}

	private void onPlayerLoggedIn(PlayerEvent.PlayerLoggedInEvent event) {
		PacketHandler.sendToClient((ServerPlayer) event.getPlayer(), new SyncPlayerSettingsMessage(BackpackSettingsManager.getPlayerBackpackSettingsTag(event.getPlayer())));
	}

	private void onBlockClick(PlayerInteractEvent.LeftClickBlock event) {
		if (event.getWorld().isClientSide) {
			return;
		}
		Player player = event.getPlayer();
		BlockPos pos = event.getPos();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
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
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
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
		ItemStack remainingStackSimulated = itemEntity.getItem().copy();
		Player player = event.getPlayer();
		Level world = player.getCommandSenderWorld();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, remainingStackSimulated, wrapper, true)).orElse(false));
		if (remainingStackSimulated.isEmpty()) {
			ItemStack remainingStack = itemEntity.getItem().copy();
			PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, remainingStack, wrapper, false)).orElse(false)
			);
			if (!itemEntity.isSilent()) {
				Random rand = itemEntity.level.random;
				itemEntity.level.playSound(null, player.getX(), player.getY(), player.getZ(), SoundEvents.ITEM_PICKUP, SoundSource.PLAYERS, 0.2F, (RandHelper.getRandomMinusOneToOne(rand) * 0.7F + 1.0F) * 2.0F);
			}
			itemEntity.setItem(ItemStack.EMPTY);
			event.setCanceled(true);
		}
	}
}
