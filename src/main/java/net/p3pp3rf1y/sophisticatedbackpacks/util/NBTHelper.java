package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;

import java.util.Optional;
import java.util.function.BiFunction;

public class NBTHelper {
	private NBTHelper() {}

	public static Optional<Integer> getInt(ItemStack stack, String tagName) {
		return getTagValue(stack, tagName, CompoundNBT::getInt);
	}

	private static <T> Optional<T> getTagValue(ItemStack stack, String tagName, BiFunction<CompoundNBT, String, T> getValue) {
		if (!stack.hasTag()) {
			return Optional.empty();
		}
		CompoundNBT tag = stack.getTag();
		//noinspection ConstantConditions
		if (!tag.contains(tagName)) {
			return Optional.empty();
		}
		return Optional.of(getValue.apply(tag, tagName));
	}

	public static Optional<CompoundNBT> getCompound(ItemStack stack, String tagName) {
		return getTagValue(stack, tagName, CompoundNBT::getCompound);
	}
}
