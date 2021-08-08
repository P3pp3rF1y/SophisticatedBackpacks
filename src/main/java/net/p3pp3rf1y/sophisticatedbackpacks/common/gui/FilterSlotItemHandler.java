package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;

import java.util.function.Supplier;

public class FilterSlotItemHandler extends SlotSuppliedHandler implements IFilterSlot {
	public FilterSlotItemHandler(Supplier<IItemHandler> itemHandlerSupplier, int slot, int xPosition, int yPosition) {
		super(itemHandlerSupplier, slot, xPosition, yPosition);
	}

	@Override
	public boolean mayPickup(PlayerEntity playerIn) {
		return false;
	}

	@Override
	public int getMaxStackSize(ItemStack stack) {
		return 1;
	}

}
