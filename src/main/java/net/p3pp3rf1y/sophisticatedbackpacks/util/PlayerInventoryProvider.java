package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public class PlayerInventoryProvider {
	private PlayerInventoryProvider() {}

	public static final String MAIN_INVENTORY = "main";
	public static final String OFFHAND_INVENTORY = "offhand";
	private static final Map<String, PlayerInventoryHandler> playerInventoryHandlers = new LinkedHashMap<>();
	private static final List<String> renderedHandlers = new ArrayList<>();

	static {
		PlayerInventoryProvider.addPlayerInventoryHandler(MAIN_INVENTORY, player -> player.inventory.mainInventory.size(),
				(player, slot) -> player.inventory.mainInventory.get(slot), (player, slot, stack) -> player.inventory.mainInventory.set(slot, stack), true, false);
		PlayerInventoryProvider.addPlayerInventoryHandler(OFFHAND_INVENTORY, player -> player.inventory.offHandInventory.size(),
				(player, slot) -> player.inventory.offHandInventory.get(slot), (player, slot, stack) -> player.inventory.offHandInventory.set(slot, stack), false, false);
		PlayerInventoryProvider.addPlayerInventoryHandler("armor", player -> 1,
				(player, slot) -> player.inventory.armorInventory.get(EquipmentSlotType.CHEST.getIndex()), (player, slot, stack) -> player.inventory.armorInventory.set(EquipmentSlotType.CHEST.getIndex(), stack), false, true);
	}

	public static void addPlayerInventoryHandler(String name, Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, PlayerInventoryHandler.IStackInSlotModifier setStackInSlot, boolean visibleInGui, boolean rendered) {
		Map<String, PlayerInventoryHandler> temp = new LinkedHashMap<>(playerInventoryHandlers);
		playerInventoryHandlers.clear();
		playerInventoryHandlers.put(name, new PlayerInventoryHandler(getSlotCount, getStackInSlot, setStackInSlot, visibleInGui));
		playerInventoryHandlers.putAll(temp);

		if (rendered) {
			ArrayList<String> tempRendered = new ArrayList<>(renderedHandlers);
			renderedHandlers.clear();
			renderedHandlers.add(name);
			renderedHandlers.addAll(tempRendered);
		}
	}

	public static Optional<ItemStack> getBackpackFromRendered(PlayerEntity player) {
		for (String handlerName : renderedHandlers) {
			PlayerInventoryHandler invHandler = playerInventoryHandlers.get(handlerName);
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem) {
					return Optional.of(slotStack);
				}
			}
		}
		return Optional.empty();
	}

	public static Optional<PlayerInventoryHandler> getPlayerInventoryHandler(String name) {
		return Optional.ofNullable(playerInventoryHandlers.get(name));
	}

	public static void runOnBackpacks(PlayerEntity player, BackpackInventorySlotConsumer backpackInventorySlotConsumer) {
		for (Map.Entry<String, PlayerInventoryHandler> entry : playerInventoryHandlers.entrySet()) {
			PlayerInventoryHandler invHandler = entry.getValue();
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem && backpackInventorySlotConsumer.accept(slotStack.copy(), entry.getKey(), slot)) {
					return;
				}
			}
		}
	}

	public interface BackpackInventorySlotConsumer {
		boolean accept(ItemStack backpack, String inventoryHandlerName, int slot);
	}
}
