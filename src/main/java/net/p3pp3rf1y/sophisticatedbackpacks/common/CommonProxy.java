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
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.event.AddReloadListenerEvent;
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
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncPlayerSettingsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.RegistryLoader;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.ServerBackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import java.util.Random;

public class CommonProxy {
	private final RegistryLoader registryLoader = new RegistryLoader();
	private final PlayerInventoryProvider playerInventoryProvider = new PlayerInventoryProvider();

	public void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		ModItems.registerHandlers(modBus);
		ModBlocks.registerHandlers(modBus);
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
		ItemStack remainingStackSimulated = itemEntity.getItem().copy();
		PlayerEntity player = event.getPlayer();
		World world = player.getCommandSenderWorld();
		playerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, remainingStackSimulated, wrapper, true)).orElse(false));
		if (remainingStackSimulated.isEmpty()) {
			ItemStack remainingStack = itemEntity.getItem().copy();
			playerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, remainingStack, wrapper, false)).orElse(false)
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
