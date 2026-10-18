package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageHostWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

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
		if (isLinkedEndpoint(backpack)) {
			return;
		}
		IBackpackWrapper backpackToKeep = BackpackWrapper.fromStack(backpack);
		List<IBackpackWrapper> duplicates = new ArrayList<>();
		PlayerInventoryProvider.get().runOnBackpacks(player, (otherBackpack, inventoryHandlerName, identifier, slot) -> {
			if (otherBackpack != backpack && !isLinkedEndpoint(otherBackpack)) {
				IBackpackWrapper wrapper = BackpackWrapper.fromStack(otherBackpack);
				wrapper.getContentsUuid().ifPresent(uuid -> {
					if (uuid.equals(backpackUuid)) {
						duplicates.add(wrapper);
					}
				});
			}
			return false;
		});
		for (IBackpackWrapper duplicate : duplicates) {
			backpackToKeep = dedupeBackpackWrappers(backpackToKeep, duplicate);
		}
	}

	public static void dedupeBackpackItemEntityInArea(ItemEntity newBackpackItemEntity) {
		ItemStack newBackpackStack = newBackpackItemEntity.getItem();
		if (!(newBackpackStack.getItem() instanceof BackpackItem) || isLinkedEndpoint(newBackpackStack)) {
			return;
		}

		IBackpackWrapper newBackpackWrapper = BackpackWrapper.fromStack(newBackpackStack);
		newBackpackWrapper.getContentsUuid().ifPresent(backpackId -> dedupeBackpackItemEntityInArea(newBackpackWrapper, newBackpackItemEntity, backpackId));
	}

	private static void dedupeBackpackItemEntityInArea(IBackpackWrapper newBackpackWrapper, ItemEntity newBackpackItemEntity, UUID backpackId) {
		IBackpackWrapper backpackToKeep = newBackpackWrapper;
		for (ItemEntity entity : newBackpackItemEntity.level().getEntitiesOfClass(ItemEntity.class, newBackpackItemEntity.getBoundingBox().inflate(10),
				Entity::isAlive)) {
			if (entity != newBackpackItemEntity) {
				backpackToKeep = dedupeMatchingEntityBackpack(backpackToKeep, backpackId, entity);
			}
		}
	}

	private static IBackpackWrapper dedupeMatchingEntityBackpack(IBackpackWrapper backpackToKeep, UUID backpackId, ItemEntity entity) {
		ItemStack entityStack = entity.getItem();
		if (!(entityStack.getItem() instanceof BackpackItem) || isLinkedEndpoint(entityStack)) {
			return backpackToKeep;
		}

		IBackpackWrapper entityBackpackWrapper = BackpackWrapper.fromStack(entityStack);
		return entityBackpackWrapper.getContentsUuid().filter(backpackId::equals).map(uuid -> dedupeBackpackWrappers(backpackToKeep, entityBackpackWrapper))
				.orElse(backpackToKeep);
	}

	public static IBackpackWrapper dedupeBackpackWrappers(IBackpackWrapper firstBackpackWrapper, IBackpackWrapper secondBackpackWrapper) {
		boolean firstLinked = isLinkedStorageWrapper(firstBackpackWrapper);
		boolean secondLinked = isLinkedStorageWrapper(secondBackpackWrapper);
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

	private static boolean isLinkedEndpoint(ItemStack stack) {
		return LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT;
	}

	private static boolean isLinkedStorageWrapper(IBackpackWrapper wrapper) {
		return wrapper instanceof BackpackLinkedStorageHostWrapper || wrapper instanceof LinkedStorageBackpackWrapper;
	}
}
