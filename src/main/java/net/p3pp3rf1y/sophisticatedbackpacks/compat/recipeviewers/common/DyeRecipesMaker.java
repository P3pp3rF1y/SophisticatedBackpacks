package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.core.NonNullList;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.DyeItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.*;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.DyeVariantPair;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SingleColorDyeRecipeSpec;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class DyeRecipesMaker {
	private DyeRecipesMaker() {
	}

	public static List<RecipeHolder<CraftingRecipe>> getMultipleColorsRecipes() {
		List<RecipeHolder<CraftingRecipe>> recipes = new ArrayList<>();
		addMultipleColorsRecipe(recipes);

		return recipes;
	}

	public static List<SingleColorDyeRecipeSpec> getSingleColorRecipeSpecs() {
		List<SingleColorDyeRecipeSpec> recipes = new ArrayList<>();
		for (Item backpackItem : getBackpackItems()) {
			List<DyeVariantPair> variants = new ArrayList<>();
			for (DyeColor color : DyeColor.values()) {
				ItemStack backpackOutput = new ItemStack(backpackItem);
				BackpackItem.setColors(backpackOutput, color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
				variants.add(new DyeVariantPair(new ItemStack(DyeItem.byColor(color)), backpackOutput));
			}
			ResourceLocation id = ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "single_color_" + BuiltInRegistries.ITEM.getKey(backpackItem).getPath());
			recipes.add(new SingleColorDyeRecipeSpec(id, List.of(new ItemStack(backpackItem)), variants));
		}
		return recipes;
	}

	private static void addMultipleColorsRecipe(List<RecipeHolder<CraftingRecipe>> recipes) {
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

			ShapedRecipePattern pattern = new ShapedRecipePattern(3, 1, ingredients, Optional.empty());
			ResourceLocation id = ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "multiple_colors_" + BuiltInRegistries.ITEM.getKey(backpackItem).getPath());
			recipes.add(new RecipeHolder<>(id, new ShapedRecipe("", CraftingBookCategory.MISC, pattern, backpackOutput)));
		}
	}

	private static List<Item> getBackpackItems() {
		return List.of(ModItems.BACKPACK.get(), ModItems.COPPER_BACKPACK.get(), ModItems.IRON_BACKPACK.get(), ModItems.GOLD_BACKPACK.get(), ModItems.DIAMOND_BACKPACK.get(), ModItems.NETHERITE_BACKPACK.get());
	}
}
