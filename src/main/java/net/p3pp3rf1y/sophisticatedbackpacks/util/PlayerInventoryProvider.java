package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

public class PlayerInventoryProvider {
	public static final String MAIN_INVENTORY = "main";
	public static final String OFFHAND_INVENTORY = "offhand";
	private static final String ARMOR_INVENTORY = "armor";

	private final Map<String, PlayerInventoryHandler> playerInventoryHandlers = new LinkedHashMap<>();
	private final List<String> renderedHandlers = new ArrayList<>();

	private boolean playerInventoryHandlersInitialized = false;
	private Consumer<PlayerEntity> playerInventoryHandlerInitCallback = player -> {};

	public PlayerInventoryProvider() {
		addPlayerInventoryHandler(MAIN_INVENTORY, player -> player.inventory.items.size(),
				(player, slot) -> player.inventory.items.get(slot), (player, slot, stack) -> player.inventory.items.set(slot, stack), true, false, false);
		addPlayerInventoryHandler(OFFHAND_INVENTORY, player -> player.inventory.offhand.size(),
				(player, slot) -> player.inventory.offhand.get(slot), (player, slot, stack) -> player.inventory.offhand.set(slot, stack), false, false, false);
		addPlayerInventoryHandler(ARMOR_INVENTORY, player -> 1,
				(player, slot) -> player.inventory.armor.get(EquipmentSlotType.CHEST.getIndex()), (player, slot, stack) -> player.inventory.armor.set(EquipmentSlotType.CHEST.getIndex(), stack), false, true, false);

	}

	public void setPlayerInventoryHandlerInitCallback(Consumer<PlayerEntity> callback) {
		playerInventoryHandlerInitCallback = callback;
	}

	public void addPlayerInventoryHandler(String name, Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, PlayerInventoryHandler.IStackInSlotModifier setStackInSlot, boolean visibleInGui, boolean rendered, boolean ownRenderer) {
		Map<String, PlayerInventoryHandler> temp = new LinkedHashMap<>(playerInventoryHandlers);
		playerInventoryHandlers.clear();
		playerInventoryHandlers.put(name, new PlayerInventoryHandler(getSlotCount, getStackInSlot, setStackInSlot, visibleInGui, ownRenderer));
		playerInventoryHandlers.putAll(temp);

		if (rendered) {
			ArrayList<String> tempRendered = new ArrayList<>(renderedHandlers);
			renderedHandlers.clear();
			renderedHandlers.add(name);
			renderedHandlers.addAll(tempRendered);
		}
	}

	public Optional<RenderInfo> getBackpackFromRendered(PlayerEntity player) {
		initialize(player);
		for (String handlerName : renderedHandlers) {
			PlayerInventoryHandler invHandler = playerInventoryHandlers.get(handlerName);
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem) {
					return invHandler.hasItsOwnRenderer() ? Optional.empty() : Optional.of(new RenderInfo(slotStack, handlerName.equals(ARMOR_INVENTORY)));
				}
			}
		}
		return Optional.empty();
	}

	private Map<String, PlayerInventoryHandler> getPlayerInventoryHandlers(PlayerEntity player) {
		initialize(player);
		return playerInventoryHandlers;
	}

	private void initialize(PlayerEntity player) {
		if (!playerInventoryHandlersInitialized) {
			playerInventoryHandlerInitCallback.accept(player);
			playerInventoryHandlersInitialized = true;
		}
	}

	public Optional<PlayerInventoryHandler> getPlayerInventoryHandler(PlayerEntity player, String name) {
		return Optional.ofNullable(getPlayerInventoryHandlers(player).get(name));
	}

	public void runOnBackpacks(PlayerEntity player, BackpackInventorySlotConsumer backpackInventorySlotConsumer) {
		for (Map.Entry<String, PlayerInventoryHandler> entry : getPlayerInventoryHandlers(player).entrySet()) {
			PlayerInventoryHandler invHandler = entry.getValue();
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem && backpackInventorySlotConsumer.accept(slotStack, entry.getKey(), slot)) {
					return;
				}
			}
		}
	}

	public interface BackpackInventorySlotConsumer {
		boolean accept(ItemStack backpack, String inventoryHandlerName, int slot);
	}

	public static class RenderInfo {
		private final ItemStack backpack;
		private final boolean isArmorSlot;

		public RenderInfo(ItemStack backpack, boolean isArmorSlot) {
			this.backpack = backpack;
			this.isArmorSlot = isArmorSlot;
		}

		public ItemStack getBackpack() {
			return backpack;
		}

		public boolean isArmorSlot() {
			return isArmorSlot;
		}
	}
}
