package net.p3pp3rf1y.sophisticatedcore.inventory;

import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.ItemHandlerHelper;

public record ItemStackKey(ItemStack stack) {
	public ItemStack getStack() {
		return stack;
	}

	public ItemStackKey(ItemStack stack) {
		this.stack = stack.copy();
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {return true;}
		if (o == null || getClass() != o.getClass()) {return false;}
		ItemStackKey that = (ItemStackKey) o;
		return ItemHandlerHelper.canItemStacksStack(stack, that.stack);
	}

	@Override
	public int hashCode() {
		//noinspection ConstantConditions - hasTag call makes sure getTag doesn't return null
		return stack.getItem().hashCode() * 31 + (stack.hasTag() ? stack.getTag().hashCode() : 0);
	}
}
