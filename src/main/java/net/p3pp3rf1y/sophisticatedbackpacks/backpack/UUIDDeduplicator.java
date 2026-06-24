package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import java.util.UUID;

/**
 * This class has logic for deduplicating backpack UUIDs because people duplicating backpack including its UUID seems to happen fairly regularly as well as some
 * mods also add enchants / legit ways to duplicate loot when mobs are killed which can again include backpack
 */

public class UUIDDeduplicator {
	private UUIDDeduplicator() {
	}

	public static void checkForDuplicateBackpacksAndRemoveTheirUUID(Player player, UUID backpackUuid, ItemStack backpack) {
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(backpackWrapper -> PlayerInventoryProvider.get().runOnBackpacks(player, (otherBackpack, inventoryHandlerName, identifier, slot) -> {
					if (otherBackpack != backpack) {
						otherBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
								.ifPresent(wrapper -> wrapper.getContentsUuid().ifPresent(uuid -> {
									if (uuid.equals(backpackUuid)) {
										dedupeBackpackWrappers(backpackWrapper, wrapper);
									}
								}));
					}
					return false;
				}));
	}

	public static void dedupeBackpackItemEntityInArea(ItemEntity newBackpackItemEntity) {
		newBackpackItemEntity.getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(newBackpackWrapper -> newBackpackWrapper
				.getContentsUuid().ifPresent(backpackId -> dedupeBackpackItemEntityInArea(newBackpackWrapper, newBackpackItemEntity, backpackId)));
	}

	private static void dedupeBackpackItemEntityInArea(IBackpackWrapper newBackpackWrapper, ItemEntity newBackpackItemEntity, UUID backpackId) {
		for (ItemEntity entity : newBackpackItemEntity.level().getEntitiesOfClass(ItemEntity.class, newBackpackItemEntity.getBoundingBox().inflate(10),
				Entity::isAlive)) {
			if (entity != newBackpackItemEntity) {
				checkEntityBackpackIdMatchAndRemoveIfItDoes(newBackpackWrapper, backpackId, entity);
			}
		}
	}

	private static boolean checkEntityBackpackIdMatchAndRemoveIfItDoes(IBackpackWrapper newBackpackWrapper, UUID newBackpackId, ItemEntity entity) {
		return entity.getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).resolve()
				.map(wrapper -> wrapper.getContentsUuid().map(backpackId -> {
					if (backpackId.equals(newBackpackId)) {
						dedupeBackpackWrappers(newBackpackWrapper, wrapper);
						return true;
					}
					return false;
				}).orElse(false)).orElse(false);
	}

	public static IBackpackWrapper dedupeBackpackWrappers(IBackpackWrapper firstBackpackWrapper, IBackpackWrapper secondBackpackWrapper) {
		if (isFirstBackpackPreferred(firstBackpackWrapper, secondBackpackWrapper)) {
			removeUuid(secondBackpackWrapper);
			return firstBackpackWrapper;
		}

		removeUuid(firstBackpackWrapper);
		return secondBackpackWrapper;
	}

	public static boolean isFirstBackpackPreferred(IBackpackWrapper firstBackpackWrapper, IBackpackWrapper secondBackpackWrapper) {
		return getInventorySize(firstBackpackWrapper) >= getInventorySize(secondBackpackWrapper);
	}

	private static int getInventorySize(IBackpackWrapper backpackWrapper) {
		return backpackWrapper.getInventoryHandler().getSlots();
	}

	private static void removeUuid(IBackpackWrapper backpackWrapper) {
		backpackWrapper.removeContentsUUIDTag();
		backpackWrapper.onContentsNbtUpdated();
	}
}
