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
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.event.entity.EntityMobGriefingEvent;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.event.entity.player.EntityItemPickupEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.ServerBackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import java.util.Random;

public class CommonProxy {
	public void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		ModItems.registerHandlers(modBus);
		ModBlocks.registerHandlers(modBus);
		MinecraftForge.EVENT_BUS.addListener(this::onItemPickup);
		MinecraftForge.EVENT_BUS.addListener(this::onLivingSpecialSpawn);
		MinecraftForge.EVENT_BUS.addListener(this::onLivingDrops);
		MinecraftForge.EVENT_BUS.addListener(this::onCauldronInteract);
		MinecraftForge.EVENT_BUS.addListener(this::onEntityMobGriefing);
		MinecraftForge.EVENT_BUS.addListener(this::onEntityLeaveWorld);
		MinecraftForge.EVENT_BUS.addListener(ServerBackpackSoundHandler::tick);
	}

	private void onCauldronInteract(PlayerInteractEvent.RightClickBlock event) {
		PlayerEntity player = event.getPlayer();
		Hand hand = event.getHand();
		ItemStack backpack = player.getHeldItem(hand);
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
		int level = state.get(CauldronBlock.LEVEL);

		LazyOptional<IBackpackWrapper> backpackWrapperCapability = backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
		if (level == 0 || backpackWrapperCapability.map(this::hasDefaultColor).orElse(true)) {
			return;
		}

		if (!world.isRemote) {
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

	private void onLivingSpecialSpawn(LivingSpawnEvent.SpecialSpawn event) {
		Entity entity = event.getEntity();
		if (entity instanceof MonsterEntity) {
			MonsterEntity monster = (MonsterEntity) entity;
			if (monster.getItemStackFromSlot(EquipmentSlotType.CHEST).isEmpty()) {
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
		World world = player.getEntityWorld();
		PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, remainingStackSimulated, wrapper, true)).orElse(false));
		if (remainingStackSimulated.isEmpty()) {
			ItemStack remainingStack = itemEntity.getItem().copy();
			PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, remainingStack, wrapper, false)).orElse(false)
			);
			if (!itemEntity.isSilent()) {
				Random rand = itemEntity.world.rand;
				itemEntity.world.playSound(null, player.getPosX(), player.getPosY(), player.getPosZ(), SoundEvents.ENTITY_ITEM_PICKUP, SoundCategory.PLAYERS, 0.2F, (RandHelper.getRandomMinusOneToOne(rand) * 0.7F + 1.0F) * 2.0F);
			}
			itemEntity.setItem(ItemStack.EMPTY);
			event.setCanceled(true);
		}
	}

}
