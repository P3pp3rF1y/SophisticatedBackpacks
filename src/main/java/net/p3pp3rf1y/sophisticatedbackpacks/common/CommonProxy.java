package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.block.CauldronBlock;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.ItemStack;
import net.minecraft.potion.EffectInstance;
import net.minecraft.potion.Effects;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.event.AddReloadListenerEvent;
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
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IAttackEntityResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModFluids;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModParticles;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncPlayerSettingsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.RegistryLoader;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.ServerBackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class CommonProxy {
	private final RegistryLoader registryLoader = new RegistryLoader();
	private final PlayerInventoryProvider playerInventoryProvider = new PlayerInventoryProvider();

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
		eventBus.addListener(this::onCauldronInteract);
		eventBus.addListener(this::onEntityMobGriefing);
		eventBus.addListener(this::onEntityLeaveWorld);
		eventBus.addListener(ServerBackpackSoundHandler::tick);
		eventBus.addListener(this::onBlockClick);
		eventBus.addListener(this::onAttackEntity);
		eventBus.addListener(EntityBackpackAdditionHandler::onLivingUpdate);
		eventBus.addListener(this::onAddReloadListener);
		eventBus.addListener(this::onPlayerLoggedIn);
		eventBus.addListener(this::onPlayerChangedDimension);
		eventBus.addListener(this::onWorldTick);
	}

	private static final int BACKPACK_COUNT_CHECK_COOLDOWN = 40;
	private long nextBackpackCountCheck = 0;

	private void onWorldTick(TickEvent.WorldTickEvent event) {
		if (event.world.isClientSide || event.phase != TickEvent.Phase.END || Boolean.FALSE.equals(Config.COMMON.nerfsConfig.tooManyBackpacksSlowness.get()) || nextBackpackCountCheck > event.world.getGameTime()) {
			return;
		}
		nextBackpackCountCheck = event.world.getGameTime() + BACKPACK_COUNT_CHECK_COOLDOWN;

		event.world.players().forEach(player -> {
			AtomicInteger numberOfBackpacks = new AtomicInteger(0);
			SophisticatedBackpacks.PROXY.getPlayerInventoryProvider().runOnBackpacks(player, (backpack, handlerName, slot) -> {
				numberOfBackpacks.incrementAndGet();
				return false;
			});
			int maxNumberOfBackpacks = Config.COMMON.nerfsConfig.maxNumberOfBackpacks.get();
			if (numberOfBackpacks.get() > maxNumberOfBackpacks) {
				int numberOfSlownessLevels = Math.min(10, (int) Math.ceil((numberOfBackpacks.get() - maxNumberOfBackpacks) * Config.COMMON.nerfsConfig.slownessLevelsPerAdditionalBackpack.get()));
				player.addEffect(new EffectInstance(Effects.MOVEMENT_SLOWDOWN, BACKPACK_COUNT_CHECK_COOLDOWN * 2, numberOfSlownessLevels - 1, false, false));
			}
		});
	}

	public PlayerInventoryProvider getPlayerInventoryProvider() {
		return playerInventoryProvider;
	}

	private void onPlayerChangedDimension(PlayerEvent.PlayerChangedDimensionEvent event) {
		PacketHandler.sendToClient((ServerPlayerEntity) event.getPlayer(), new SyncPlayerSettingsMessage(BackpackSettingsManager.getPlayerBackpackSettingsTag(event.getPlayer())));
	}

	private void onPlayerLoggedIn(PlayerEvent.PlayerLoggedInEvent event) {
		PacketHandler.sendToClient((ServerPlayerEntity) event.getPlayer(), new SyncPlayerSettingsMessage(BackpackSettingsManager.getPlayerBackpackSettingsTag(event.getPlayer())));
	}

	private void onAddReloadListener(AddReloadListenerEvent event) {
		event.addListener(registryLoader);
	}

	private void onCauldronInteract(PlayerInteractEvent.RightClickBlock event) {
		PlayerEntity player = event.getPlayer();
		Hand hand = event.getHand();
		ItemStack backpack = player.getItemInHand(hand);
		if (!(backpack.getItem() instanceof BackpackItem)) {
			return;
		}

		BlockPos pos = event.getPos();
		World world = event.getWorld();
		BlockState state = world.getBlockState(pos);
		Block block = state.getBlock();
		if (block != Blocks.CAULDRON) {
			return;
		}
		int level = state.getValue(CauldronBlock.LEVEL);

		LazyOptional<IBackpackWrapper> backpackWrapperCapability = backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
		if (level == 0 || backpackWrapperCapability.map(this::hasDefaultColor).orElse(true)) {
			return;
		}

		if (!world.isClientSide) {
			backpackWrapperCapability.ifPresent(w -> {
				w.setColors(BackpackWrapper.DEFAULT_CLOTH_COLOR, BackpackWrapper.DEFAULT_BORDER_COLOR);
				((CauldronBlock) block).setWaterLevel(world, pos, state, level - 1);
			});
		}

		event.setCanceled(true);
		event.setCancellationResult(ActionResultType.SUCCESS);
	}

	private boolean hasDefaultColor(IBackpackWrapper wrapper) {
		return wrapper.getBorderColor() == BackpackWrapper.DEFAULT_BORDER_COLOR && wrapper.getClothColor() == BackpackWrapper.DEFAULT_CLOTH_COLOR;
	}

	private void onBlockClick(PlayerInteractEvent.LeftClickBlock event) {
		if (event.getWorld().isClientSide) {
			return;
		}
		PlayerEntity player = event.getPlayer();
		BlockPos pos = event.getPos();
		playerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
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
		PlayerEntity player = event.getPlayer();
		if (player.level.isClientSide) {
			return;
		}
		playerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
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
		if (entity instanceof MonsterEntity) {
			MonsterEntity monster = (MonsterEntity) entity;
			if (monster.getItemBySlot(EquipmentSlotType.CHEST).isEmpty()) {
				EntityBackpackAdditionHandler.addBackpack(monster);
			}
		}
	}

	private void onLivingDrops(LivingDropsEvent event) {
		EntityBackpackAdditionHandler.handleBackpackDrop(event);
	}

	private void onEntityMobGriefing(EntityMobGriefingEvent event) {
		if (event.getEntity() instanceof CreeperEntity) {
			EntityBackpackAdditionHandler.removeBeneficialEffects((CreeperEntity) event.getEntity());
		}
	}

	private void onEntityLeaveWorld(EntityLeaveWorldEvent event) {
		if (!(event.getEntity() instanceof MonsterEntity)) {
			return;
		}
		EntityBackpackAdditionHandler.removeBackpackUuid((MonsterEntity) event.getEntity());
	}

	private void onItemPickup(EntityItemPickupEvent event) {
		ItemEntity itemEntity = event.getItem();
		if (itemEntity.getItem().isEmpty()) {
			return;
		}

		AtomicReference<ItemStack> remainingStackSimulated = new AtomicReference<>(itemEntity.getItem().copy());
		PlayerEntity player = event.getPlayer();
		World world = player.getCommandSenderWorld();
		playerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> {
					remainingStackSimulated.set(InventoryHelper.runPickupOnBackpack(world, remainingStackSimulated.get(), wrapper, true));
					return remainingStackSimulated.get().isEmpty();
				}).orElse(false));
		if (remainingStackSimulated.get().isEmpty()) {
			ItemStack remainingStack = itemEntity.getItem().copy();
			playerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, player, remainingStack, wrapper, false).isEmpty()).orElse(false)
			);
			if (!itemEntity.isSilent()) {
				Random rand = itemEntity.level.random;
				itemEntity.level.playSound(null, player.getX(), player.getY(), player.getZ(), SoundEvents.ITEM_PICKUP, SoundCategory.PLAYERS, 0.2F, (RandHelper.getRandomMinusOneToOne(rand) * 0.7F + 1.0F) * 2.0F);
			}
			itemEntity.setItem(ItemStack.EMPTY);
			event.setCanceled(true);
		}
	}

}
