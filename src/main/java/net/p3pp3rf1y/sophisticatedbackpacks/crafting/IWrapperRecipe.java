package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.item.crafting.IRecipe;

public interface IWrapperRecipe<T extends IRecipe<?>> {
	T getCompose();
}
