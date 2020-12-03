package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.ByteNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.IntNBT;
import net.minecraft.nbt.LongNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraft.util.IStringSerializable;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

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

	public static <T extends Enum<T>> Optional<T> getEnumConstant(ItemStack stack, String key, Function<String, T> deserialize) {
		return getTagValue(stack, key, (t, k) -> deserialize.apply(t.getString(k)));
	}

	public static Optional<Boolean> getBoolean(ItemStack stack, String key) {
		return getTagValue(stack, key, CompoundNBT::getBoolean);
	}

	public static Optional<Long> getLong(ItemStack stack, String key) {
		return getTagValue(stack, key, CompoundNBT::getLong);
	}

	public static void setBoolean(ItemStack stack, String key, boolean value) {
		stack.setTagInfo(key, ByteNBT.valueOf(value));
	}

	public static <T extends Enum<T> & IStringSerializable> void setEnumConstant(ItemStack stack, String key, T enumConstant) {
		stack.setTagInfo(key, StringNBT.valueOf(enumConstant.getString()));
	}

	public static CompoundNBT putBoolean(CompoundNBT tag, String key, boolean value) {
		tag.putBoolean(key, value);
		return tag;
	}

	public static <T extends Enum<T> & IStringSerializable> CompoundNBT putEnumConstant(CompoundNBT tag, String key, T enumConstant) {
		tag.putString(key, enumConstant.getString());
		return tag;
	}

	public static void setLong(ItemStack stack, String key, long value) {
		stack.setTagInfo(key, LongNBT.valueOf(value));
	}

	public static void setInteger(ItemStack stack, String key, int value) {
		stack.setTagInfo(key, IntNBT.valueOf(value));
	}
}
