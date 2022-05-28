package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.loading.FMLEnvironment;
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
	public static final String ARMOR_INVENTORY = "armor";

	private final Map<String, PlayerInventoryHandler> playerInventoryHandlers = new LinkedHashMap<>();
	private final List<String> renderedHandlers = new ArrayList<>();

	private boolean playerInventoryHandlersInitialized = false;
	private Consumer<Player> playerInventoryHandlerInitCallback = player -> {};

	private static final PlayerInventoryProvider serverProvider = new PlayerInventoryProvider();
	private static final PlayerInventoryProvider clientProvider = new PlayerInventoryProvider();

	public static PlayerInventoryProvider get() {
		if (FMLEnvironment.dist == Dist.CLIENT) {
			return clientProvider;
		} else {
			return serverProvider;
		}
	}

	private PlayerInventoryProvider() {
		addPlayerInventoryHandler(MAIN_INVENTORY, player -> player.getInventory().items.size(),
				(player, slot) -> player.getInventory().items.get(slot), (player, slot, stack) -> player.getInventory().items.set(slot, stack), true, false, false, false);
		addPlayerInventoryHandler(OFFHAND_INVENTORY, player -> player.getInventory().offhand.size(),
				(player, slot) -> player.getInventory().offhand.get(slot), (player, slot, stack) -> player.getInventory().offhand.set(slot, stack), false, false, false, false);
		addPlayerInventoryHandler(ARMOR_INVENTORY, player -> 1,
				(player, slot) -> player.getInventory().armor.get(EquipmentSlot.CHEST.getIndex()), (player, slot, stack) -> player.getInventory().armor.set(EquipmentSlot.CHEST.getIndex(), stack), false, true, false, true);
	}

	public void setPlayerInventoryHandlerInitCallback(Consumer<Player> callback) {
		playerInventoryHandlerInitCallback = callback;
		//there seems to be a bug where something triggers one of the backpack codes too early before curio has time
		// to register its callback which causes the callback not to be called if initialized wasn't set to false here
		playerInventoryHandlersInitialized = false;
	}

	public void removePlayerInventoryHandlersStartingWith(String prefix) {
		playerInventoryHandlers.entrySet().removeIf(e -> e.getKey().startsWith(prefix));
		renderedHandlers.removeIf(e -> e.startsWith(prefix));
	}

	public void addPlayerInventoryHandler(String name, Function<Player, Integer> getSlotCount, BiFunction<Player, Integer, ItemStack> getStackInSlot, PlayerInventoryHandler.IStackInSlotModifier setStackInSlot, boolean visibleInGui, boolean rendered, boolean ownRenderer, boolean accessibleByAnotherPlayer) {
		Map<String, PlayerInventoryHandler> temp = new LinkedHashMap<>(playerInventoryHandlers);
		playerInventoryHandlers.clear();
		playerInventoryHandlers.put(name, new PlayerInventoryHandler(getSlotCount, getStackInSlot, setStackInSlot, visibleInGui, ownRenderer, accessibleByAnotherPlayer));
		playerInventoryHandlers.putAll(temp);

		if (rendered) {
			ArrayList<String> tempRendered = new ArrayList<>(renderedHandlers);
			renderedHandlers.clear();
			renderedHandlers.add(name);
			renderedHandlers.addAll(tempRendered);
		}
	}

	public Optional<RenderInfo> getBackpackFromRendered(Player player) {
		initialize(player);
		for (String handlerName : renderedHandlers) {
			PlayerInventoryHandler invHandler = playerInventoryHandlers.get(handlerName);
			if (invHandler == null) {
				return Optional.empty();
			}
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem) {
					return invHandler.hasItsOwnRenderer() ? Optional.empty() : Optional.of(new RenderInfo(slotStack, handlerName.equals(ARMOR_INVENTORY)));
				}
			}
		}
		return Optional.empty();
	}

	private Map<String, PlayerInventoryHandler> getPlayerInventoryHandlers(Player player) {
		initialize(player);
		return playerInventoryHandlers;
	}

	private void initialize(Player player) {
		if (!playerInventoryHandlersInitialized) {
			playerInventoryHandlerInitCallback.accept(player);
			playerInventoryHandlersInitialized = true;
		}
	}

	public Optional<PlayerInventoryHandler> getPlayerInventoryHandler(Player player, String name) {
		return Optional.ofNullable(getPlayerInventoryHandlers(player).get(name));
	}

	public void runOnBackpacks(Player player, BackpackInventorySlotConsumer backpackInventorySlotConsumer) {
		runOnBackpacks(player, backpackInventorySlotConsumer, false);
	}

	public void runOnBackpacks(Player player, BackpackInventorySlotConsumer backpackInventorySlotConsumer, boolean onlyAccessibleByAnotherPlayer) {
		for (Map.Entry<String, PlayerInventoryHandler> entry : getPlayerInventoryHandlers(player).entrySet()) {
			PlayerInventoryHandler invHandler = entry.getValue();
			if (onlyAccessibleByAnotherPlayer && !invHandler.isAccessibleByAnotherPlayer()) {
				continue;
			}

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
