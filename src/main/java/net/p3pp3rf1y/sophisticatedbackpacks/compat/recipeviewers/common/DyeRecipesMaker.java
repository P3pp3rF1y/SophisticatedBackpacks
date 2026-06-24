package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ItemStackTemplate;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.*;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.ClientRecipeHelper;
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
				BackpackWrapper.fromStack(backpackOutput).setColors(color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
				variants.add(new DyeVariantPair(new ItemStack(dyeItem(color)), backpackOutput));
			}
			Identifier id = Identifier.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID,
					"single_color_" + BuiltInRegistries.ITEM.getKey(backpackItem).getPath());
			recipes.add(new SingleColorDyeRecipeSpec(id, List.of(new ItemStack(backpackItem)), variants));
		}
		return recipes;
	}

	private static void addMultipleColorsRecipe(List<RecipeHolder<CraftingRecipe>> recipes) {
		for (Item backpackItem : getBackpackItems()) {
			List<Optional<Ingredient>> ingredients = new ArrayList<>();
			ingredients.add(Optional.of(Ingredient.of(dyeItem(DyeColor.YELLOW))));
			ingredients.add(Optional.of(Ingredient.of(backpackItem)));
			ingredients.add(Optional.empty());
			ingredients.add(Optional.of(Ingredient.of(dyeItem(DyeColor.LIME))));
			ingredients.add(Optional.of(Ingredient.of(dyeItem(DyeColor.BLUE))));
			ingredients.add(Optional.of(Ingredient.of(dyeItem(DyeColor.BLACK))));

			ItemStack backpackOutput = new ItemStack(backpackItem);
			int clothColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_MAIN_COLOR,
					List.of(DyeColor.YELLOW, DyeColor.LIME));
			int trimColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_ACCENT_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR,
					List.of(DyeColor.BLUE, DyeColor.BLACK));

			BackpackWrapper.fromStack(backpackOutput).setColors(clothColor, trimColor);

			ShapedRecipePattern pattern = new ShapedRecipePattern(3, 2, ingredients, Optional.empty());
			Identifier id = Identifier.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID,
					"multiple_colors_" + BuiltInRegistries.ITEM.getKey(backpackItem).getPath());
			recipes.add(new RecipeHolder<>(ClientRecipeHelper.recipeKey(id), new ShapedRecipe(new Recipe.CommonInfo(true),
					new CraftingRecipe.CraftingBookInfo(CraftingBookCategory.MISC, ""), pattern, ItemStackTemplate.fromNonEmptyStack(backpackOutput))));
		}
	}

	private static Item dyeItem(DyeColor color) {
		return switch (color) {
			case WHITE -> Items.WHITE_DYE;
			case ORANGE -> Items.ORANGE_DYE;
			case MAGENTA -> Items.MAGENTA_DYE;
			case LIGHT_BLUE -> Items.LIGHT_BLUE_DYE;
			case YELLOW -> Items.YELLOW_DYE;
			case LIME -> Items.LIME_DYE;
			case PINK -> Items.PINK_DYE;
			case GRAY -> Items.GRAY_DYE;
			case LIGHT_GRAY -> Items.LIGHT_GRAY_DYE;
			case CYAN -> Items.CYAN_DYE;
			case PURPLE -> Items.PURPLE_DYE;
			case BLUE -> Items.BLUE_DYE;
			case BROWN -> Items.BROWN_DYE;
			case GREEN -> Items.GREEN_DYE;
			case RED -> Items.RED_DYE;
			case BLACK -> Items.BLACK_DYE;
		};
	}

	private static List<Item> getBackpackItems() {
		return List.of(ModItems.BACKPACK.get(), ModItems.COPPER_BACKPACK.get(), ModItems.IRON_BACKPACK.get(), ModItems.GOLD_BACKPACK.get(),
				ModItems.DIAMOND_BACKPACK.get(), ModItems.NETHERITE_BACKPACK.get());
	}
}
