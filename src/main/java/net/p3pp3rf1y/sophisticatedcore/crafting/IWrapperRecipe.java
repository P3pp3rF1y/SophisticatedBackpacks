package net.p3pp3rf1y.sophisticatedcore.crafting;

import net.minecraft.world.item.crafting.Recipe;

public interface IWrapperRecipe<T extends Recipe<?>> {
	T getCompose();
}
