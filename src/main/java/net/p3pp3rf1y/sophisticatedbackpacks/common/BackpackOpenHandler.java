package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.ItemStack;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public class BackpackOpenHandler {
	private BackpackOpenHandler() {}

	private static final Map<String, PlayerInventoryHandler> backpackInventoryHandlers = new LinkedHashMap<>();

	public static void addBackpackInventoryHandler(String name, Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, boolean visibleInGui) {
		backpackInventoryHandlers.put(name, new PlayerInventoryHandler(getSlotCount, getStackInSlot, visibleInGui));
	}

	public static final String MAIN_INVENTORY = "main";

	public static final String OFFHAND_INVENTORY = "offhand";

	static {
		addBackpackInventoryHandler(MAIN_INVENTORY, player -> player.inventory.mainInventory.size(), (player, slot) -> player.inventory.mainInventory.get(slot), true);
		addBackpackInventoryHandler(OFFHAND_INVENTORY, player -> player.inventory.offHandInventory.size(), (player, slot) -> player.inventory.offHandInventory.get(slot), false);
		addBackpackInventoryHandler("armor", player -> player.inventory.armorInventory.size(), (player, slot) -> player.inventory.armorInventory.get(slot), true);
	}

	public static Optional<PlayerInventoryHandler> getBackpackInventoryHandler(String name) {
		return Optional.ofNullable(backpackInventoryHandlers.get(name));
	}

	public static void handle(PlayerEntity player) {
		ArrayList<String> names = new ArrayList<>(BackpackOpenHandler.backpackInventoryHandlers.keySet());
		Collections.reverse(names);
		for (String name : names) {
			PlayerInventoryHandler invHandler = backpackInventoryHandlers.get(name);
			for (int slot = 0; slot < invHandler.getSlotCount(player); slot++) {
				ItemStack slotStack = invHandler.getStackInSlot(player, slot);
				if (slotStack.getItem() instanceof BackpackItem) {
					int finalSlot = slot;
					NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, name, finalSlot), slotStack.getDisplayName()),
							buf -> {
								buf.writeString(name);
								buf.writeInt(finalSlot);
							});
					return;
				}
			}
		}
	}

	public static class PlayerInventoryHandler {
		private final Function<PlayerEntity, Integer> getSlotCount;
		private final BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot;
		private final boolean visibleInGui;

		public PlayerInventoryHandler(Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, boolean visibleInGui) {
			this.getSlotCount = getSlotCount;
			this.getStackInSlot = getStackInSlot;
			this.visibleInGui = visibleInGui;
		}

		public int getSlotCount(PlayerEntity player) {
			return getSlotCount.apply(player);
		}

		public ItemStack getStackInSlot(PlayerEntity player, int slot) {
			return getStackInSlot.apply(player, slot);
		}

		public boolean isVisibleInGui() {
			return visibleInGui;
		}
	}
}
