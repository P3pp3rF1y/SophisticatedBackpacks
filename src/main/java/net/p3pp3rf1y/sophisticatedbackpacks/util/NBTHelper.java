package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.INBT;
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
		return getTagValue(stack, "", key, getValue);
	}

	private static <T> Optional<T> getTagValue(ItemStack stack, String parentKey, String key, BiFunction<CompoundNBT, String, T> getValue) {
		CompoundNBT tag = stack.getTag();

		if (tag == null) {
			return Optional.empty();
		}

		if (!parentKey.isEmpty()) {
			INBT parentTag = tag.get(parentKey);
			if (!(parentTag instanceof CompoundNBT)) {
				return Optional.empty();
			}
			tag = (CompoundNBT) parentTag;
		}

		if (!tag.contains(key)) {
			return Optional.empty();
		}

		return Optional.of(getValue.apply(tag, key));
	}

	public static Optional<CompoundNBT> getCompound(ItemStack stack, String parentKey, String tagName) {
		return getTagValue(stack, parentKey, tagName, CompoundNBT::getCompound);
	}

	public static Optional<CompoundNBT> getCompound(ItemStack stack, String tagName) {
		return getTagValue(stack, tagName, CompoundNBT::getCompound);
	}

	public static <T extends Enum<T>> Optional<T> getEnumConstant(ItemStack stack, String parentKey, String key, Function<String, T> deserialize) {
		return getTagValue(stack, parentKey, key, (t, k) -> deserialize.apply(t.getString(k)));
	}

	public static <T extends Enum<T>> Optional<T> getEnumConstant(ItemStack stack, String key, Function<String, T> deserialize) {
		return getTagValue(stack, key, (t, k) -> deserialize.apply(t.getString(k)));
	}

	public static Optional<Boolean> getBoolean(ItemStack stack, String parentKey, String key) {
		return getTagValue(stack, parentKey, key, CompoundNBT::getBoolean);
	}

	public static Optional<Boolean> getBoolean(ItemStack stack, String key) {
		return getTagValue(stack, key, CompoundNBT::getBoolean);
	}

	public static Optional<Long> getLong(ItemStack stack, String key) {
		return getTagValue(stack, key, CompoundNBT::getLong);
	}

	public static void setCompoundNBT(ItemStack stack, String parentKey, String key, CompoundNBT tag) {
		if (parentKey.isEmpty()) {
			stack.getOrCreateTag().put(key, tag);
			return;
		}
		stack.getOrCreateChildTag(parentKey).put(key, tag);
	}

	public static void setBoolean(ItemStack stack, String parentKey, String key, boolean value) {
		if (parentKey.isEmpty()) {
			setBoolean(stack, key, value);
			return;
		}
		putBoolean(stack.getOrCreateChildTag(parentKey), key, value);
	}

	public static void setBoolean(ItemStack stack, String key, boolean value) {
		putBoolean(stack.getOrCreateTag(), key, value);
	}

	public static <T extends Enum<T> & IStringSerializable> void setEnumConstant(ItemStack stack, String parentKey, String key, T enumConstant) {
		if (parentKey.isEmpty()) {
			setEnumConstant(stack, key, enumConstant);
			return;
		}
		putEnumConstant(stack.getOrCreateChildTag(parentKey), key, enumConstant);
	}

	public static <T extends Enum<T> & IStringSerializable> void setEnumConstant(ItemStack stack, String key, T enumConstant) {
		putEnumConstant(stack.getOrCreateTag(), key, enumConstant);
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
		stack.getOrCreateTag().putLong(key, value);
	}

	public static void setInteger(ItemStack stack, String key, int value) {
		stack.getOrCreateTag().putInt(key, value);
	}
}
