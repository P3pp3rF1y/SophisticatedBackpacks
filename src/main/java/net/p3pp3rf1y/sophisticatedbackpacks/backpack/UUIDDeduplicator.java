package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;

import java.util.UUID;

/**
 * This class has logic for deduplicating backpack UUIDs because people duplicating backpack including its UUID seems to happen fairly regularly
 * as well as some mods also add enchants / legit ways to duplicate loot when mobs are killed which can again include backpack
 */

public class UUIDDeduplicator {
	private UUIDDeduplicator() {}

	public static void checkForDuplicateBackpacksAndRemoveTheirUUID(Player player, UUID backpackUuid, ItemStack backpack) {
		PlayerInventoryProvider.get().runOnBackpacks(player, (otherBackpack, inventoryHandlerName, identifier, slot) -> {
			if (otherBackpack != backpack) {
				otherBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
						.ifPresent(wrapper -> wrapper.getContentsUuid().ifPresent(uuid -> {
							if (uuid.equals(backpackUuid)) {
								wrapper.removeContentsUUIDTag();
								wrapper.onContentsNbtUpdated();
							}
						}));
			}
			return false;
		});
	}

	public static void dedupeBackpackItemEntityInArea(ItemEntity newBackpackItemEntity) {
		newBackpackItemEntity.getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(newBackpackWrapper -> newBackpackWrapper.getContentsUuid()
						.ifPresent(backpackId -> dedupeBackpackItemEntityInArea(newBackpackWrapper, newBackpackItemEntity, backpackId))
				);
	}

	private static void dedupeBackpackItemEntityInArea(IBackpackWrapper newBackpackWrapper, ItemEntity newBackpackItemEntity, UUID backpackId) {
		for (ItemEntity entity : newBackpackItemEntity.level.getEntitiesOfClass(ItemEntity.class, newBackpackItemEntity.getBoundingBox().inflate(10), Entity::isAlive)) {
			if (checkEntityBackpackIdMatchAndRemoveIfItDoes(newBackpackWrapper, backpackId, entity)) {
				break;
			}
		}
	}

	private static boolean checkEntityBackpackIdMatchAndRemoveIfItDoes(IBackpackWrapper newBackpackWrapper, UUID newBackpackId, ItemEntity entity) {
		return entity.getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).resolve().flatMap(IStorageWrapper::getContentsUuid).map(backpackId -> {
			if (backpackId.equals(newBackpackId)) {
				newBackpackWrapper.removeContentsUUIDTag();
				newBackpackWrapper.onContentsNbtUpdated();
				return true;
			}
			return false;
		}).orElse(false);
	}
}