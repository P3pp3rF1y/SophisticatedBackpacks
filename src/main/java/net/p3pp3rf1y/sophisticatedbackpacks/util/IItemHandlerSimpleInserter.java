package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;

public interface IItemHandlerSimpleInserter extends IItemHandlerModifiable {
	ItemStack insertItem(ItemStack stack, boolean simulate);
}
