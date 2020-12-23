package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.player.EntityItemPickupEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
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
	}

	private void onItemPickup(EntityItemPickupEvent event) {
		ItemEntity itemEntity = event.getItem();
		ItemStack remainingStackSimulated = itemEntity.getItem().copy();
		PlayerEntity player = event.getPlayer();
		World world = player.getEntityWorld();
		PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
				.map(wrapper -> InventoryHelper.runPickupOnBackpack(world, remainingStackSimulated, wrapper, true)).orElse(false));
		if (remainingStackSimulated.isEmpty()) {
			ItemStack remainingStack = itemEntity.getItem().copy();
			PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
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
