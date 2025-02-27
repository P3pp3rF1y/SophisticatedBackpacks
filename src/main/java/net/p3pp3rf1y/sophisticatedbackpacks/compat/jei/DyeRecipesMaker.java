package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import net.minecraft.core.NonNullList;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.*;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class DyeRecipesMaker {
	private DyeRecipesMaker() {
	}

	public static List<RecipeHolder<CraftingRecipe>> getRecipes() {
		List<RecipeHolder<CraftingRecipe>> recipes = new ArrayList<>();
		addSingleColorRecipes(recipes);
		addMultipleColorsRecipe(recipes);

		return recipes;
	}

	private static void addMultipleColorsRecipe(List<RecipeHolder<CraftingRecipe>> recipes) {
		NonNullList<Ingredient> ingredients = NonNullList.create();
		ingredients.add(Ingredient.of(DyeColor.YELLOW.getTag()));
		ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
		ingredients.add(Ingredient.EMPTY);
		ingredients.add(Ingredient.of(DyeColor.LIME.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLUE.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLACK.getTag()));

		ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
		int clothColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_MAIN_COLOR, List.of(
				DyeColor.YELLOW, DyeColor.LIME
		));
		int trimColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_ACCENT_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR, List.of(
				DyeColor.BLUE, DyeColor.BLACK
		));

		BackpackWrapper.fromStack(backpackOutput).setColors(clothColor, trimColor);

		ShapedRecipePattern pattern = new ShapedRecipePattern(3, 1, ingredients, Optional.empty());
		ResourceLocation id = ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "multiple_colors");
		recipes.add(new RecipeHolder<>(id, new ShapedRecipe("", CraftingBookCategory.MISC, pattern, backpackOutput)));
	}

	private static void addSingleColorRecipes(List<RecipeHolder<CraftingRecipe>> recipes) {
		for (DyeColor color : DyeColor.values()) {
			ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
			BackpackWrapper.fromStack(backpackOutput).setColors(color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
			NonNullList<Ingredient> ingredients = NonNullList.create();
			ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
			ingredients.add(Ingredient.of(color.getTag()));

			ShapedRecipePattern pattern = new ShapedRecipePattern(1, 2, ingredients, Optional.empty());
			ResourceLocation id = ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "single_color_" + color.getSerializedName());
			recipes.add(new RecipeHolder<>(id, new ShapedRecipe("", CraftingBookCategory.MISC, pattern, backpackOutput)));
		}
	}
}
