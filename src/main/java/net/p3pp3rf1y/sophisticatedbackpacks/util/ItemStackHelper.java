package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;

import javax.annotation.Nullable;
import java.util.Objects;

public class ItemStackHelper {
	private ItemStackHelper() {}

	public static boolean areItemStackTagsEqualIgnoreDurability(ItemStack stackA, ItemStack stackB) {
		if (stackA.isEmpty() && stackB.isEmpty()) {
			return true;
		} else if (!stackA.isEmpty() && !stackB.isEmpty()) {
			if (stackA.getTag() == null && stackB.getTag() != null) {
				return false;
			} else {
				return (stackA.getTag() == null || areTagsEqualIgnoreDurability(stackA.getTag(), stackB.getTag())) && stackA.areCapsCompatible(stackB);
			}
		} else {
			return false;
		}
	}

	public static boolean areTagsEqualIgnoreDurability(CompoundTag tagA, @Nullable CompoundTag tagB) {
		if (tagA == tagB) {
			return true;
		}
		if (tagB == null || tagA.size() != tagB.size()) {
			return false;
		}

		for (String key : tagA.getAllKeys()) {
			if (!tagB.contains(key)) {
				return false;
			}
			if (key.equals("Damage")) {
				continue;
			}
			if (!Objects.equals(tagA.get(key), tagB.get(key))) {
				return false;
			}
		}
		return true;
	}
}
