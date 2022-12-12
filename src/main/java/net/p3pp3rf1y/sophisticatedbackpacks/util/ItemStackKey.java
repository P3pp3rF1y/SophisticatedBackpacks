package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import javax.annotation.Nullable;
import java.lang.reflect.Field;

public class ItemStackKey {
	public ItemStack getStack() {
		return stack;
	}

	private final ItemStack stack;

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
		return getHashCode(stack);
	}

	public static int getHashCode(ItemStack stack) {
		int hash = stack.getItem().hashCode();
		if (stack.hasTag()) {
			//noinspection ConstantConditions - hasTag call makes sure getTag doesn't return null
			hash = hash * 31 + stack.getTag().hashCode();
		}
		CompoundNBT capNbt = getCapNbt(stack);
		if (capNbt != null && !capNbt.isEmpty()) {
			hash = hash * 31 + capNbt.hashCode();
		}
		return hash;
	}

	private static final Field CAP_NBT = ObfuscationReflectionHelper.findField(ItemStack.class, "capNBT");

	@Nullable
	private static CompoundNBT getCapNbt(ItemStack stack) {
		try {
			return (CompoundNBT) CAP_NBT.get(stack);
		}
		catch (IllegalAccessException e) {
			SophisticatedBackpacks.LOGGER.error("Error getting capNBT of stack ", e);
			return null;
		}
	}
}
