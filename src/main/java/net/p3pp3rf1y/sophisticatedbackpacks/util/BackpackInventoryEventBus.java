package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class BackpackInventoryEventBus {
	private static final Map<UUID, IInventorySlotUpdateListener> slotUpdateListeners = new HashMap<>();

	public static void registerListener(UUID playerUuid, IInventorySlotUpdateListener listener) {
		slotUpdateListeners.put(playerUuid, listener);
	}

	public static void unregisterListener(UUID playerUuid) {
		slotUpdateListeners.remove(playerUuid);
	}

	public static void onSlotUpdate(UUID playerUuid, String handlerName, int backpackInSlot, int slot, ItemStack newStack) {
		if (slotUpdateListeners.containsKey(playerUuid)) {
			slotUpdateListeners.get(playerUuid).onSlotUpdate(handlerName, backpackInSlot, slot, newStack);
		}
	}

	public interface IInventorySlotUpdateListener {
		void onSlotUpdate(String handlerName, int backpackInSlot, int slot, ItemStack newStack);
	}
}
