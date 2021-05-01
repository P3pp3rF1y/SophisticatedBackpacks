package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;

import java.util.function.BiFunction;
import java.util.function.Function;

public class PlayerInventoryHandler {
	private final Function<PlayerEntity, Integer> getSlotCount;
	private final BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot;
	private final IStackInSlotModifier setStackInSlot;
	private final boolean visibleInGui;
	private final boolean ownRenderer;

	public PlayerInventoryHandler(Function<PlayerEntity, Integer> getSlotCount, BiFunction<PlayerEntity, Integer, ItemStack> getStackInSlot, IStackInSlotModifier setStackInSlot, boolean visibleInGui, boolean ownRenderer) {
		this.getSlotCount = getSlotCount;
		this.getStackInSlot = getStackInSlot;
		this.setStackInSlot = setStackInSlot;
		this.visibleInGui = visibleInGui;
		this.ownRenderer = ownRenderer;
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

	public void setStackInSlot(PlayerEntity player, int slot, ItemStack stack) {
		setStackInSlot.accept(player, slot, stack);
	}

	public boolean hasItsOwnRenderer() {
		return ownRenderer;
	}

	public interface IStackInSlotModifier {
		void accept(PlayerEntity player, int slot, ItemStack stack);
	}
}
