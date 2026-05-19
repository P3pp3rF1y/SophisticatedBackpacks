package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.core.NonNullList;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.DyeItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.DyeVariantPair;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SingleColorDyeRecipeSpec;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class DyeRecipesMaker {
	private DyeRecipesMaker() {}

	public static List<CraftingRecipe> getRecipes() {
		return getRecipes(r -> r);
	}

	public static List<CraftingRecipe> getMultipleColorsRecipes() {
		List<CraftingRecipe> recipes = new ArrayList<>();
		addMultipleColorsRecipe(recipes);

		return recipes;
	}

	public static <R> List<R> getRecipes(Function<ShapedRecipe, R> transformRecipe) {
		List<R> recipes = new ArrayList<>();
		addSingleColorRecipes(recipes, transformRecipe);
		addMultipleColorsRecipe(recipes, transformRecipe);

		return recipes;
	}

	private static <R> void addMultipleColorsRecipe(List<R> recipes, Function<ShapedRecipe, R> transformRecipe) {
		NonNullList<Ingredient> ingredients = NonNullList.create();
		ingredients.add(Ingredient.of(DyeColor.YELLOW.getTag()));
		ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
		ingredients.add(Ingredient.EMPTY);
		ingredients.add(Ingredient.of(DyeColor.LIME.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLUE.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLACK.getTag()));

		ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
		int clothColor = ColorHelper.calculateColor(BackpackItem.DEFAULT_MAIN_COLOR, BackpackItem.DEFAULT_MAIN_COLOR, List.of(
				DyeColor.BLUE, DyeColor.YELLOW, DyeColor.LIME
		));
		int trimColor = ColorHelper.calculateColor(BackpackItem.DEFAULT_ACCENT_COLOR, BackpackItem.DEFAULT_ACCENT_COLOR, List.of(
				DyeColor.BLUE, DyeColor.BLACK
		));

		BackpackItem.setColors(backpackOutput, clothColor, trimColor);

		ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "multiple_colors");
		recipes.add(transformRecipe.apply(new ShapedRecipe(id, "", CraftingBookCategory.MISC, 3, 1, ingredients, backpackOutput)));
	}

	private static <R> void addSingleColorRecipes(List<R> recipes, Function<ShapedRecipe, R> transformRecipe) {
		for (DyeColor color : DyeColor.values()) {
			ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "single_color_" + color.getSerializedName());
			ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
			BackpackItem.setColors(backpackOutput, ColorHelper.getColor(color.getTextureDiffuseColors()), ColorHelper.getColor(color.getTextureDiffuseColors()));
			NonNullList<Ingredient> ingredients = NonNullList.create();
			ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
			ingredients.add(Ingredient.of(color.getTag()));
			recipes.add(transformRecipe.apply(new ShapedRecipe(id, "", CraftingBookCategory.MISC, 1, 2, ingredients, backpackOutput)));
		}
	}

	public static List<SingleColorDyeRecipeSpec> getSingleColorRecipeSpecs() {
		List<SingleColorDyeRecipeSpec> recipes = new ArrayList<>();
		for (Item backpackItem : getBackpackItems()) {
			List<DyeVariantPair> variants = new ArrayList<>();
			for (DyeColor color : DyeColor.values()) {
				ItemStack backpackOutput = new ItemStack(backpackItem);
				int colorValue = ColorHelper.getColor(color.getTextureDiffuseColors());
				BackpackItem.setColors(backpackOutput, colorValue, colorValue);
				variants.add(new DyeVariantPair(new ItemStack(DyeItem.byColor(color)), backpackOutput));
			}
			ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "single_color_" + BuiltInRegistries.ITEM.getKey(backpackItem).getPath());
			recipes.add(new SingleColorDyeRecipeSpec(id, List.of(new ItemStack(backpackItem)), variants));
		}
		return recipes;
	}

	private static void addMultipleColorsRecipe(List<CraftingRecipe> recipes) {
		for (Item backpackItem : getBackpackItems()) {
			NonNullList<Ingredient> ingredients = NonNullList.create();
			ingredients.add(Ingredient.of(DyeColor.YELLOW.getTag()));
			ingredients.add(Ingredient.of(backpackItem));
			ingredients.add(Ingredient.EMPTY);
			ingredients.add(Ingredient.of(DyeColor.LIME.getTag()));
			ingredients.add(Ingredient.of(DyeColor.BLUE.getTag()));
			ingredients.add(Ingredient.of(DyeColor.BLACK.getTag()));

			ItemStack backpackOutput = new ItemStack(backpackItem);
			int clothColor = ColorHelper.calculateColor(BackpackItem.DEFAULT_MAIN_COLOR, BackpackItem.DEFAULT_MAIN_COLOR, List.of(
					DyeColor.YELLOW, DyeColor.LIME
			));
			int trimColor = ColorHelper.calculateColor(BackpackItem.DEFAULT_ACCENT_COLOR, BackpackItem.DEFAULT_ACCENT_COLOR, List.of(
					DyeColor.BLUE, DyeColor.BLACK
			));

			BackpackItem.setColors(backpackOutput, clothColor, trimColor);

			ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "multiple_colors_" + BuiltInRegistries.ITEM.getKey(backpackItem).getPath());
			recipes.add(new ShapedRecipe(id, "", CraftingBookCategory.MISC, 3, 1, ingredients, backpackOutput));
		}
	}

	private static List<Item> getBackpackItems() {
		return List.of(ModItems.BACKPACK.get(), ModItems.COPPER_BACKPACK.get(), ModItems.IRON_BACKPACK.get(), ModItems.GOLD_BACKPACK.get(), ModItems.DIAMOND_BACKPACK.get(), ModItems.NETHERITE_BACKPACK.get());
	}
}
