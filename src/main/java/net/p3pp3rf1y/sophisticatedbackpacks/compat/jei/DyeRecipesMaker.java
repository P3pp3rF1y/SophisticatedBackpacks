package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.ICraftingRecipe;
import net.minecraft.item.crafting.Ingredient;
import net.minecraft.item.crafting.ShapedRecipe;
import net.minecraft.item.crafting.ShapelessRecipe;
import net.minecraft.util.NonNullList;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class DyeRecipesMaker {
	private DyeRecipesMaker() {}
	public static Collection<ICraftingRecipe> getRecipes() {
		Set<ICraftingRecipe> recipes = new HashSet<>();
		addSingleColorRecipes(recipes);
		addTwoColorsRecipe(recipes);

		return recipes;
	}

	private static void addTwoColorsRecipe(Set<ICraftingRecipe> recipes) {
		NonNullList<Ingredient> ingredients = NonNullList.create();
		ingredients.add(Ingredient.fromTag(DyeColor.YELLOW.getTag()));
		ingredients.add(Ingredient.fromItems(ModItems.BACKPACK.get()));
		ingredients.add(Ingredient.fromTag(DyeColor.BLUE.getTag()));

		ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
		backpackOutput.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.setColors(DyeColor.YELLOW.getColorValue(), DyeColor.BLUE.getColorValue()));

		ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "two_colors");
		recipes.add(new ShapedRecipe(id, "", 3, 1, ingredients, backpackOutput));
	}

	private static void addSingleColorRecipes(Set<ICraftingRecipe> recipes) {
		for (DyeColor color : DyeColor.values()) {
			ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "single_color_" + color.getString());
			ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
			backpackOutput.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.setColors(color.getColorValue(), color.getColorValue()));
			NonNullList<Ingredient> ingredients = NonNullList.create();
			ingredients.add(Ingredient.fromItems(ModItems.BACKPACK.get()));
			ingredients.add(Ingredient.fromTag(color.getTag()));
			recipes.add(new ShapelessRecipe(id, "", backpackOutput, ingredients));
		}
	}
}
