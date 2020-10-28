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
		PlayerInventoryProvider.addPlayerInventoryHandler(MAIN_INVENTORY, player -> player.inventory.mainInventory.size(), (player, slot) -> player.inventory.mainInventory.get(slot), true);
		PlayerInventoryProvider.addPlayerInventoryHandler(OFFHAND_INVENTORY, player -> player.inventory.offHandInventory.size(), (player, slot) -> player.inventory.offHandInventory.get(slot), false);
		PlayerInventoryProvider.addPlayerInventoryHandler("armor", player -> player.inventory.armorInventory.size(), (player, slot) -> player.inventory.armorInventory.get(slot), true);
	}

	public static void addPlayerInventoryHandler(String name, Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, boolean visibleInGui) {
		playerInventoryHandlers.put(name, new PlayerInventoryHandler(getSlotCount, getStackInSlot, visibleInGui));
	}

	public static Optional<PlayerInventoryHandler> getPlayerInventoryHandler(String name) {
		return Optional.ofNullable(playerInventoryHandlers.get(name));
	}

	public static void runOnFirstBackpack(PlayerEntity player, BackpackInventorySlotConsumer backpackInventorySlotConsumer) {
		ArrayList<String> names = new ArrayList<>(PlayerInventoryProvider.playerInventoryHandlers.keySet());
		Collections.reverse(names);
		for (String name : names) {
			PlayerInventoryHandler invHandler = PlayerInventoryProvider.playerInventoryHandlers.get(name);
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem) {
					backpackInventorySlotConsumer.accept(slotStack, name, slot);
					return;
				}
			}
		}
	}

	public interface BackpackInventorySlotConsumer {
		void accept(ItemStack backpack, String inventoryHandlerName, int slot);
	}
}
