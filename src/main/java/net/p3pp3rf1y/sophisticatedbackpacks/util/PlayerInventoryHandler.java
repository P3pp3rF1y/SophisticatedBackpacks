package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

import java.util.Collections;
import java.util.Set;
import java.util.function.Function;

public class PlayerInventoryHandler {
	public static final Set<String> SINGLE_IDENTIFIER = Collections.singleton("");
	private final Function<Long, Set<String>> identifiersGetter;
	private final SlotCountGetter slotCountGetter;
	private final SlotStackGetter slotStackGetter;
	private final boolean visibleInGui;
	private final boolean ownRenderer;
	private final boolean accessibleByAnotherPlayer;

	public PlayerInventoryHandler(Function<Long, Set<String>> identifiersGetter, SlotCountGetter slotCountGetter, SlotStackGetter slotStackGetter, boolean visibleInGui, boolean ownRenderer, boolean accessibleByAnotherPlayer) {
		this.identifiersGetter = identifiersGetter;
		this.slotCountGetter = slotCountGetter;
		this.slotStackGetter = slotStackGetter;
		this.visibleInGui = visibleInGui;
		this.ownRenderer = ownRenderer;
		this.accessibleByAnotherPlayer = accessibleByAnotherPlayer;
	}

	public int getSlotCount(Player player, String identifier) {
		return slotCountGetter.getSlotCount(player, identifier);
	}

	public ItemStack getStackInSlot(Player player, String identifier, int slot) {
		return slotStackGetter.getStackInSlot(player, identifier, slot);
	}

	public boolean isVisibleInGui() {
		return visibleInGui;
	}

	public Set<String> getIdentifiers(long gameTime) {
		return identifiersGetter.apply(gameTime);
	}

	public boolean hasItsOwnRenderer() {
		return ownRenderer;
	}

	public boolean isAccessibleByAnotherPlayer() {
		return accessibleByAnotherPlayer;
	}

	public interface SlotCountGetter {
		int getSlotCount(Player player, String identifier);
	}

	public interface SlotStackGetter {
		ItemStack getStackInSlot(Player player, String identifier, int slot);
	}

}
