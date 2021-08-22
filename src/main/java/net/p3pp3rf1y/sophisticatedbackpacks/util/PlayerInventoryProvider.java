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
import java.util.function.Function;

public class PlayerInventoryProvider {
	private PlayerInventoryProvider() {}

	public static final String MAIN_INVENTORY = "main";
	public static final String OFFHAND_INVENTORY = "offhand";
	private static final Map<String, PlayerInventoryHandler> playerInventoryHandlers = new LinkedHashMap<>();
	private static final List<String> renderedHandlers = new ArrayList<>();
	private static final String ARMOR_INVENTORY = "armor";

	private static boolean playerInventoryHandlersInitialized = false;
	private static Runnable playerInventoryHandlerInitCallback = () -> {};

	static {
		PlayerInventoryProvider.addPlayerInventoryHandler(MAIN_INVENTORY, player -> player.inventory.items.size(),
				(player, slot) -> player.inventory.items.get(slot), (player, slot, stack) -> player.inventory.items.set(slot, stack), true, false, false);
		PlayerInventoryProvider.addPlayerInventoryHandler(OFFHAND_INVENTORY, player -> player.inventory.offhand.size(),
				(player, slot) -> player.inventory.offhand.get(slot), (player, slot, stack) -> player.inventory.offhand.set(slot, stack), false, false, false);
		PlayerInventoryProvider.addPlayerInventoryHandler(ARMOR_INVENTORY, player -> 1,
				(player, slot) -> player.inventory.armor.get(EquipmentSlotType.CHEST.getIndex()), (player, slot, stack) -> player.inventory.armor.set(EquipmentSlotType.CHEST.getIndex(), stack), false, true, false);
	}

	public static void setPlayerInventoryHandlerInitCallback(Runnable callback) {
		playerInventoryHandlerInitCallback = callback;
	}

	public static void addPlayerInventoryHandler(String name, Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, PlayerInventoryHandler.IStackInSlotModifier setStackInSlot, boolean visibleInGui, boolean rendered, boolean ownRenderer) {
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

	public static Optional<RenderInfo> getBackpackFromRendered(PlayerEntity player) {
		initialize();
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

	private static Map<String, PlayerInventoryHandler> getPlayerInventoryHandlers() {
		initialize();
		return playerInventoryHandlers;
	}

	private static void initialize() {
		if (!playerInventoryHandlersInitialized) {
			playerInventoryHandlersInitialized = true;
			playerInventoryHandlerInitCallback.run();
		}
	}

	public static Optional<PlayerInventoryHandler> getPlayerInventoryHandler(String name) {
		return Optional.ofNullable(getPlayerInventoryHandlers().get(name));
	}

	public static void runOnBackpacks(PlayerEntity player, BackpackInventorySlotConsumer backpackInventorySlotConsumer) {
		for (Map.Entry<String, PlayerInventoryHandler> entry : getPlayerInventoryHandlers().entrySet()) {
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
