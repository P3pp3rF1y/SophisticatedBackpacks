package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

import java.util.Collections;
import java.util.Set;
import java.util.function.Function;

public class PlayerInventoryHandler {
	public static final Set<String> SINGLE_IDENTIFIER = Collections.singleton("");
	private final Function<Player, Set<String>> identifiersGetter;
	private final SlotCountGetter slotCountGetter;
	private final SlotStackGetter slotStackGetter;
	private final boolean visibleInGui;
	private final boolean ownLayerRenderer;
	private final boolean accessibleByAnotherPlayer;
	private final VisibleInWorldGetter visibleInWorldGetter;

	public PlayerInventoryHandler(Function<Player, Set<String>> identifiersGetter, SlotCountGetter slotCountGetter, SlotStackGetter slotStackGetter,
			boolean visibleInGui, boolean ownLayerRenderer, boolean accessibleByAnotherPlayer, VisibleInWorldGetter visibleInWorldGetter) {
		this.identifiersGetter = identifiersGetter;
		this.slotCountGetter = slotCountGetter;
		this.slotStackGetter = slotStackGetter;
		this.visibleInGui = visibleInGui;
		this.ownLayerRenderer = ownLayerRenderer;
		this.accessibleByAnotherPlayer = accessibleByAnotherPlayer;
		this.visibleInWorldGetter = visibleInWorldGetter;
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

	public Set<String> getIdentifiers(Player player) {
		return identifiersGetter.apply(player);
	}

	public boolean hasItsOwnLayerRenderer() {
		return ownLayerRenderer;
	}

	public boolean isAccessibleByAnotherPlayer() {
		return accessibleByAnotherPlayer;
	}

	public boolean isVisibleInWorld(Player player, String identifier, int slot) {
		return visibleInWorldGetter.isVisibleInWorld(player, identifier, slot);
	}

	public interface SlotCountGetter {
		int getSlotCount(Player player, String identifier);
	}

	public interface SlotStackGetter {
		ItemStack getStackInSlot(Player player, String identifier, int slot);
	}

	public interface VisibleInWorldGetter {
		VisibleInWorldGetter DEFAULT = (player, identifier, slot) -> true;
		boolean isVisibleInWorld(Player player, String identifier, int slot);
	}
}
