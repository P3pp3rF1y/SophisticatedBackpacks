package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.world.item.crafting.AbstractCookingRecipe;

public interface ICookingUpgrade<T extends AbstractCookingRecipe> {
	CookingLogic<T> getCookingLogic();
}
