package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.player.EntityItemPickupEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import java.util.Random;

public class CommonProxy {
	public void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		ModItems.registerHandlers(modBus);
		MinecraftForge.EVENT_BUS.addListener(this::onItemPickup);
	}

	private void onItemPickup(EntityItemPickupEvent event) {
		ItemEntity itemEntity = event.getItem();
		ItemStack remainingStackSimulated = itemEntity.getItem().copy();
		PlayerEntity player = event.getPlayer();
		PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> runPickupOnBackpack(remainingStackSimulated, new BackpackWrapper(backpack), true));
		if (remainingStackSimulated.isEmpty()) {
			ItemStack remainingStack = itemEntity.getItem().copy();
			PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryHandlerName, slot) -> {
						BackpackWrapper backpackWrapper = new BackpackWrapper(backpack);
						backpackWrapper.setNotificationData(player.getUniqueID(), inventoryHandlerName, slot);
						return runPickupOnBackpack(remainingStack, backpackWrapper, false);
					}
			);
			if (!itemEntity.isSilent()) {
				Random rand = itemEntity.world.rand;
				itemEntity.world.playSound(null, player.getPosX(), player.getPosY(), player.getPosZ(), SoundEvents.ENTITY_ITEM_PICKUP, SoundCategory.PLAYERS, 0.2F, (RandHelper.getRandomMinusOneToOne(rand) * 0.7F + 1.0F) * 2.0F);
			}
			itemEntity.remove();
			event.setCanceled(true);
		}
	}

	private boolean runPickupOnBackpack(ItemStack remainingStack, BackpackWrapper backpackWrapper, boolean simulate) {
		return backpackWrapper.getUpgradeHandler().getUpgrade(upgrade -> upgrade.getItem() instanceof IPickupResponseUpgrade)
				.map(upgrade -> {
					ItemStack ret = ((IPickupResponseUpgrade) upgrade.getItem()).pickup(remainingStack, backpackWrapper, simulate);
					remainingStack.setCount(ret.getCount());
					return remainingStack.isEmpty();
				}).orElse(false);
	}
}
