package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;

import java.util.Optional;
import java.util.function.BiFunction;

public class NBTHelper {
	private NBTHelper() {}

	public static Optional<Integer> getInt(ItemStack stack, String key) {
		return getTagValue(stack, key, CompoundNBT::getInt);
	}

	private static <T> Optional<T> getTagValue(ItemStack stack, String key, BiFunction<CompoundNBT, String, T> getValue) {
		if (!stack.hasTag()) {
			return Optional.empty();
		}
		CompoundNBT tag = stack.getTag();
		//noinspection ConstantConditions
		if (!tag.contains(key)) {
			return Optional.empty();
		}
		return Optional.of(getValue.apply(tag, key));
	}

	public static Optional<CompoundNBT> getCompound(ItemStack stack, String tagName) {
		return getTagValue(stack, tagName, CompoundNBT::getCompound);
	}

	public static CompoundNBT putBoolean(CompoundNBT tag, String key, boolean value) {
		tag.putBoolean(key, value);
		return tag;
	}

	public static Optional<Boolean> getBoolean(ItemStack stack, String key) {
		return getTagValue(stack, key, CompoundNBT::getBoolean);
	}
}
