package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.item.crafting.ShapedRecipe;

public interface IShapeBasedRecipe extends IRecipe<CraftingInventory> {
	ShapedRecipe getCompose();
}
