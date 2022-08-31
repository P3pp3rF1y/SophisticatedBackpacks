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
import java.util.Set;
import java.util.function.Function;

public class PlayerInventoryProvider {
	public static final String MAIN_INVENTORY = "main";
	public static final String OFFHAND_INVENTORY = "offhand";
	public static final String ARMOR_INVENTORY = "armor";

	private final Map<String, PlayerInventoryHandler> playerInventoryHandlers = new LinkedHashMap<>();
	private final List<String> renderedHandlers = new ArrayList<>();

	public PlayerInventoryProvider() {
		addPlayerInventoryHandler(MAIN_INVENTORY, gameTime -> PlayerInventoryHandler.SINGLE_IDENTIFIER, (player, identifier) -> player.inventory.items.size(),
				(player, identifier, slot) -> player.inventory.items.get(slot), true, false, false);
		addPlayerInventoryHandler(OFFHAND_INVENTORY, gameTime -> PlayerInventoryHandler.SINGLE_IDENTIFIER, (player, identifier) -> player.inventory.offhand.size(),
				(player, identifier, slot) -> player.inventory.offhand.get(slot), false, false, false);
		addPlayerInventoryHandler(ARMOR_INVENTORY, gameTime -> PlayerInventoryHandler.SINGLE_IDENTIFIER, (player, identifier) -> 1,
				(player, identifier, slot) -> player.inventory.armor.get(EquipmentSlotType.CHEST.getIndex()), false, true, false);

	}

	public void addPlayerInventoryHandler(String name, Function<Long, Set<String>> identifiersGetter, PlayerInventoryHandler.SlotCountGetter slotCountGetter, PlayerInventoryHandler.SlotStackGetter slotStackGetter, boolean visibleInGui, boolean rendered, boolean ownRenderer) {
		Map<String, PlayerInventoryHandler> temp = new LinkedHashMap<>(playerInventoryHandlers);
		playerInventoryHandlers.clear();
		playerInventoryHandlers.put(name, new PlayerInventoryHandler(identifiersGetter, slotCountGetter, slotStackGetter, visibleInGui, ownRenderer));
		playerInventoryHandlers.putAll(temp);

		if (rendered) {
			ArrayList<String> tempRendered = new ArrayList<>(renderedHandlers);
			renderedHandlers.clear();
			renderedHandlers.add(name);
			renderedHandlers.addAll(tempRendered);
		}
	}

	public Optional<RenderInfo> getBackpackFromRendered(PlayerEntity player) {
		for (String handlerName : renderedHandlers) {
			PlayerInventoryHandler invHandler = playerInventoryHandlers.get(handlerName);
			if (invHandler == null) {
				return Optional.empty();
			}
			for (String identifier : invHandler.getIdentifiers(player.level.getGameTime())) {
				for (int slot = 0; slot < invHandler.getSlotCount(player, identifier); slot++) {
					ItemStack slotStack = invHandler.getStackInSlot(player, identifier, slot);
					if (slotStack.getItem() instanceof BackpackItem) {
						return invHandler.hasItsOwnRenderer() ? Optional.empty() : Optional.of(new RenderInfo(slotStack, handlerName.equals(ARMOR_INVENTORY)));
					}
				}
			}
		}
		return Optional.empty();
	}

	private Map<String, PlayerInventoryHandler> getPlayerInventoryHandlers() {
		return playerInventoryHandlers;
	}

	public Optional<PlayerInventoryHandler> getPlayerInventoryHandler(String name) {
		return Optional.ofNullable(getPlayerInventoryHandlers().get(name));
	}

	public void runOnBackpacks(PlayerEntity player, BackpackInventorySlotConsumer backpackInventorySlotConsumer) {
		for (Map.Entry<String, PlayerInventoryHandler> entry : getPlayerInventoryHandlers().entrySet()) {
			PlayerInventoryHandler invHandler = entry.getValue();
			for (String identifier : invHandler.getIdentifiers(player.level.getGameTime())) {
				for (int slot = 0; slot < invHandler.getSlotCount(player, identifier); slot++) {
					ItemStack slotStack = invHandler.getStackInSlot(player, identifier, slot);
					if (slotStack.getItem() instanceof BackpackItem && backpackInventorySlotConsumer.accept(slotStack, entry.getKey(), identifier, slot)) {
						return;
					}
				}
			}
		}
	}

	public interface BackpackInventorySlotConsumer {
		boolean accept(ItemStack backpack, String inventoryHandlerName, String identifier, int slot);
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
