package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;

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

	public PlayerInventoryHandler(Function<Long, Set<String>> identifiersGetter, SlotCountGetter slotCountGetter, SlotStackGetter slotStackGetter, boolean visibleInGui, boolean ownRenderer) {
		this.identifiersGetter = identifiersGetter;
		this.slotCountGetter = slotCountGetter;
		this.slotStackGetter = slotStackGetter;
		this.visibleInGui = visibleInGui;
		this.ownRenderer = ownRenderer;
	}

	public int getSlotCount(PlayerEntity player, String identifier) {
		return slotCountGetter.getSlotCount(player, identifier);
	}

	public ItemStack getStackInSlot(PlayerEntity player, String identifier, int slot) {
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

	public interface IStackInSlotModifier {
		void accept(PlayerEntity player, int slot, ItemStack stack);
	}

	public interface SlotCountGetter {
		int getSlotCount(PlayerEntity player, String identifier);
	}

	public interface SlotStackGetter {
		ItemStack getStackInSlot(PlayerEntity player, String identifier, int slot);
	}
}
