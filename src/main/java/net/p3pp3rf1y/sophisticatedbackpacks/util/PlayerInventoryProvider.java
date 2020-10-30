package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public class PlayerInventoryProvider {
	private PlayerInventoryProvider() {}

	public static final String MAIN_INVENTORY = "main";
	public static final String OFFHAND_INVENTORY = "offhand";
	private static final Map<String, PlayerInventoryHandler> playerInventoryHandlers = new LinkedHashMap<>();

	static {
		PlayerInventoryProvider.addPlayerInventoryHandler(MAIN_INVENTORY, player -> player.inventory.mainInventory.size(),
				(player, slot) -> player.inventory.mainInventory.get(slot), true, (player, slot, stack) -> player.inventory.mainInventory.set(slot, stack));
		PlayerInventoryProvider.addPlayerInventoryHandler(OFFHAND_INVENTORY, player -> player.inventory.offHandInventory.size(),
				(player, slot) -> player.inventory.offHandInventory.get(slot), false, (player, slot, stack) -> player.inventory.offHandInventory.set(slot, stack));
		PlayerInventoryProvider.addPlayerInventoryHandler("armor", player -> player.inventory.armorInventory.size(),
				(player, slot) -> player.inventory.armorInventory.get(slot), false, (player, slot, stack) -> player.inventory.armorInventory.set(slot, stack));
	}

	public static void addPlayerInventoryHandler(String name, Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, boolean visibleInGui, PlayerInventoryHandler.IStackInSlotModifier setStackInSlot) {
		playerInventoryHandlers.put(name, new PlayerInventoryHandler(getSlotCount, getStackInSlot, setStackInSlot, visibleInGui));
	}

	public static Optional<PlayerInventoryHandler> getPlayerInventoryHandler(String name) {
		return Optional.ofNullable(playerInventoryHandlers.get(name));
	}

	public static void runOnBackpacks(PlayerEntity player, BackpackInventorySlotConsumer backpackInventorySlotConsumer) {
		runOnBackpacks(player, backpackInventorySlotConsumer, false);
	}

	public static void runOnBackpacks(PlayerEntity player, BackpackInventorySlotConsumer backpackInventorySlotConsumer, boolean inPriorityOrder) {
		ArrayList<String> names = new ArrayList<>(playerInventoryHandlers.keySet());
		if (inPriorityOrder) {
			Collections.reverse(names);
		}
		for (String name : names) {
			PlayerInventoryHandler invHandler = playerInventoryHandlers.get(name);
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem && backpackInventorySlotConsumer.accept(slotStack.copy(), name, slot)) {
					return;
				}
			}
		}
	}

	public interface BackpackInventorySlotConsumer {
		boolean accept(ItemStack backpack, String inventoryHandlerName, int slot);
	}
}
