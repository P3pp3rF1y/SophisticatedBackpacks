package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageEndpointAccessProvider;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

public class BackpackLinkedStorageEndpointAccessProvider implements ILinkedStorageEndpointAccessProvider {
	@Override
	public boolean hasGroupEndpoint(ServerPlayer player, LinkedStorageGroupManager manager, UUID groupId) {
		AtomicBoolean hasGroupEndpoint = new AtomicBoolean();
		PlayerInventoryProvider.get().runOnBackpacks(player, (stack, inventoryName, identifier, slot) -> {
			boolean groupEndpoint = isGroupEndpoint(stack, manager, groupId);
			hasGroupEndpoint.set(groupEndpoint);
			return groupEndpoint;
		});
		return hasGroupEndpoint.get() || hasGroupEndpointInOpenMenu(player, manager, groupId);
	}

	private static boolean hasGroupEndpointInOpenMenu(ServerPlayer player, LinkedStorageGroupManager manager, UUID groupId) {
		if (!(player.containerMenu instanceof BackpackContainer backpackMenu)) {
			return false;
		}
		InventoryHandler inventory = backpackMenu.getStorageWrapper().getInventoryHandler();
		for (int slot = 0; slot < inventory.getSlots(); slot++) {
			if (isGroupEndpoint(inventory.getStackInSlot(slot), manager, groupId)) {
				return true;
			}
		}
		return false;
	}

	private static boolean isGroupEndpoint(ItemStack stack, LinkedStorageGroupManager manager, UUID groupId) {
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		return endpoint != null && LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT
				&& endpoint.groupId().equals(groupId) && manager.isEndpointMember(groupId, endpoint.endpointId());
	}
}
