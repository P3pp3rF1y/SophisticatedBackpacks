package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import net.minecraft.item.ItemStack;

import java.util.function.Predicate;

interface CacheableStackPredicate extends Predicate<ItemStack> {
	default boolean preventsCaching(ItemStack stack) {
		return false;
	}
}
