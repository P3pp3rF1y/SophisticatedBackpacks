package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;

import java.util.function.BiFunction;
import java.util.function.Function;

public class PlayerInventoryHandler {
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
