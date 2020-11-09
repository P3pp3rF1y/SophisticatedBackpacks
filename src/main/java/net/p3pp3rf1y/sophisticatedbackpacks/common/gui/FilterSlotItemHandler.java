package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.minecraftforge.items.SlotItemHandler;

public class FilterSlotItemHandler extends SlotItemHandler {
	public FilterSlotItemHandler(ItemStackHandler itemHandler, int i, int xPosition, int yPosition) {super(itemHandler, i, xPosition, yPosition);}

	@Override
	public boolean canTakeStack(PlayerEntity playerIn) {
		return false;
	}

	@Override
	public int getItemStackLimit(ItemStack stack) {
		return 1;
	}
}
