package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

import java.util.function.BiFunction;
import java.util.function.Function;

public class PlayerInventoryHandler {
	private final Function<Player, Integer> getSlotCount;
	private final BiFunction<Player, Integer, ItemStack> getStackInSlot;
	private final IStackInSlotModifier setStackInSlot;
	private final boolean visibleInGui;
	private final boolean ownRenderer;

	public PlayerInventoryHandler(Function<Player, Integer> getSlotCount, BiFunction<Player, Integer, ItemStack> getStackInSlot, IStackInSlotModifier setStackInSlot, boolean visibleInGui, boolean ownRenderer) {
		this.getSlotCount = getSlotCount;
		this.getStackInSlot = getStackInSlot;
		this.setStackInSlot = setStackInSlot;
		this.visibleInGui = visibleInGui;
		this.ownRenderer = ownRenderer;
	}

	public int getSlotCount(Player player) {
		return getSlotCount.apply(player);
	}

	public ItemStack getStackInSlot(Player player, int slot) {
		return getStackInSlot.apply(player, slot);
	}

	public boolean isVisibleInGui() {
		return visibleInGui;
	}

	public void setStackInSlot(Player player, int slot, ItemStack stack) {
		setStackInSlot.accept(player, slot, stack);
	}

	public boolean hasItsOwnRenderer() {
		return ownRenderer;
	}

	public interface IStackInSlotModifier {
		void accept(Player player, int slot, ItemStack stack);
	}
}
