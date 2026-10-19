package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * This class has logic for deduplicating backpack UUIDs because people duplicating backpack including its UUID seems to happen fairly regularly as well as some
 * mods also add enchants / legit ways to duplicate loot when mobs are killed which can again include backpack
 */

public class UUIDDeduplicator {
	private UUIDDeduplicator() {
	}

	public static void checkForDuplicateBackpacksAndRemoveTheirUUID(Player player, UUID backpackUuid, ItemStack backpack) {
		if (LinkedStorageStackData.getEndpoint(backpack) != null) {
			return;
		}
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> {
			List<IBackpackWrapper> duplicates = new ArrayList<>();
			PlayerInventoryProvider.get().runOnBackpacks(player, (otherBackpack, inventoryHandlerName, identifier, slot) -> {
				if (otherBackpack != backpack && LinkedStorageStackData.getEndpoint(otherBackpack) == null) {
					otherBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
							.ifPresent(wrapper -> wrapper.getContentsUuid().ifPresent(uuid -> {
								if (uuid.equals(backpackUuid)) {
									duplicates.add(wrapper);
								}
							}));
				}
				return false;
			});
			IBackpackWrapper winner = backpackWrapper;
			for (IBackpackWrapper duplicate : duplicates) {
				winner = dedupeBackpackWrappers(winner, duplicate);
			}
		});
	}

	public static void dedupeBackpackItemEntityInArea(ItemEntity newBackpackItemEntity) {
		if (LinkedStorageStackData.getEndpoint(newBackpackItemEntity.getItem()) != null) {
			return;
		}
		newBackpackItemEntity.getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(newBackpackWrapper -> newBackpackWrapper
				.getContentsUuid().ifPresent(backpackId -> dedupeBackpackItemEntityInArea(newBackpackWrapper, newBackpackItemEntity, backpackId)));
	}

	private static void dedupeBackpackItemEntityInArea(IBackpackWrapper newBackpackWrapper, ItemEntity newBackpackItemEntity, UUID backpackId) {
		IBackpackWrapper winner = newBackpackWrapper;
		for (ItemEntity entity : newBackpackItemEntity.level().getEntitiesOfClass(ItemEntity.class, newBackpackItemEntity.getBoundingBox().inflate(10),
				Entity::isAlive)) {
			if (entity != newBackpackItemEntity && LinkedStorageStackData.getEndpoint(entity.getItem()) == null) {
				winner = checkEntityBackpackIdMatchAndRemoveIfItDoes(winner, backpackId, entity);
			}
		}
	}

	private static IBackpackWrapper checkEntityBackpackIdMatchAndRemoveIfItDoes(IBackpackWrapper newBackpackWrapper, UUID newBackpackId, ItemEntity entity) {
		return entity
				.getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).resolve().map(wrapper -> wrapper.getContentsUuid()
						.filter(newBackpackId::equals).map(backpackId -> dedupeBackpackWrappers(newBackpackWrapper, wrapper)).orElse(newBackpackWrapper))
				.orElse(newBackpackWrapper);
	}

	public static IBackpackWrapper dedupeBackpackWrappers(IBackpackWrapper firstBackpackWrapper, IBackpackWrapper secondBackpackWrapper) {
		boolean firstLinked = firstBackpackWrapper.getLinkedStorageEndpoint().isPresent();
		boolean secondLinked = secondBackpackWrapper.getLinkedStorageEndpoint().isPresent();
		if (firstLinked || secondLinked) {
			return firstLinked && !secondLinked ? secondBackpackWrapper : firstBackpackWrapper;
		}
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
