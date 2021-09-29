package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import net.minecraft.core.NonNullList;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ColorHelper;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DyeRecipesMaker {
	private DyeRecipesMaker() {}

	public static Collection<CraftingRecipe> getRecipes() {
		Set<CraftingRecipe> recipes = new HashSet<>();
		addSingleColorRecipes(recipes);
		addMultipleColorsRecipe(recipes);

		return recipes;
	}

	private static void addMultipleColorsRecipe(Set<CraftingRecipe> recipes) {
		NonNullList<Ingredient> ingredients = NonNullList.create();
		ingredients.add(Ingredient.of(DyeColor.YELLOW.getTag()));
		ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
		ingredients.add(Ingredient.EMPTY);
		ingredients.add(Ingredient.of(DyeColor.LIME.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLUE.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLACK.getTag()));

		ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
		int clothColor = BackpackDyeRecipe.calculateColor(BackpackWrapper.DEFAULT_CLOTH_COLOR, BackpackWrapper.DEFAULT_CLOTH_COLOR, List.of(
				DyeColor.BLUE, DyeColor.YELLOW, DyeColor.LIME
		));
		int trimColor = BackpackDyeRecipe.calculateColor(BackpackWrapper.DEFAULT_BORDER_COLOR, BackpackWrapper.DEFAULT_BORDER_COLOR, List.of(
				DyeColor.BLUE, DyeColor.BLACK
		));

		backpackOutput.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.setColors(clothColor, trimColor));

		ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "multiple_colors");
		recipes.add(new ShapedRecipe(id, "", 3, 1, ingredients, backpackOutput));
	}

	private static void addSingleColorRecipes(Set<CraftingRecipe> recipes) {
		for (DyeColor color : DyeColor.values()) {
			ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "single_color_" + color.getSerializedName());
			ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
			backpackOutput.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(
					wrapper -> wrapper.setColors(ColorHelper.getColor(color.getTextureDiffuseColors()), ColorHelper.getColor(color.getTextureDiffuseColors())));
			NonNullList<Ingredient> ingredients = NonNullList.create();
			ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
			ingredients.add(Ingredient.of(color.getTag()));
			recipes.add(new ShapedRecipe(id, "", 1, 2, ingredients, backpackOutput));
		}
	}
}
