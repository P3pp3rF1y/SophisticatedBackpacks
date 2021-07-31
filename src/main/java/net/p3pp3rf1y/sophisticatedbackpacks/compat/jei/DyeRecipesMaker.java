package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import com.google.common.collect.ImmutableList;
import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.ICraftingRecipe;
import net.minecraft.item.crafting.Ingredient;
import net.minecraft.item.crafting.ShapedRecipe;
import net.minecraft.util.NonNullList;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class DyeRecipesMaker {
	private DyeRecipesMaker() {}

	public static Collection<ICraftingRecipe> getRecipes() {
		Set<ICraftingRecipe> recipes = new HashSet<>();
		addSingleColorRecipes(recipes);
		addMultipleColorsRecipe(recipes);

		return recipes;
	}

	private static void addMultipleColorsRecipe(Set<ICraftingRecipe> recipes) {
		NonNullList<Ingredient> ingredients = NonNullList.create();
		ingredients.add(Ingredient.of(DyeColor.YELLOW.getTag()));
		ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
		ingredients.add(Ingredient.EMPTY);
		ingredients.add(Ingredient.of(DyeColor.LIME.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLUE.getTag()));
		ingredients.add(Ingredient.of(DyeColor.BLACK.getTag()));

		ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
		int clothColor = BackpackDyeRecipe.calculateColor(BackpackWrapper.DEFAULT_CLOTH_COLOR, BackpackWrapper.DEFAULT_CLOTH_COLOR, ImmutableList.of(
				DyeColor.BLUE, DyeColor.YELLOW, DyeColor.LIME
		));
		int trimColor = BackpackDyeRecipe.calculateColor(BackpackWrapper.DEFAULT_BORDER_COLOR, BackpackWrapper.DEFAULT_BORDER_COLOR, ImmutableList.of(
				DyeColor.BLUE, DyeColor.BLACK
		));

		backpackOutput.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.setColors(clothColor, trimColor));

		ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "multiple_colors");
		recipes.add(new ShapedRecipe(id, "", 3, 1, ingredients, backpackOutput));
	}

	private static void addSingleColorRecipes(Set<ICraftingRecipe> recipes) {
		for (DyeColor color : DyeColor.values()) {
			ResourceLocation id = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "single_color_" + color.getSerializedName());
			ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
			backpackOutput.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.setColors(color.getColorValue(), color.getColorValue()));
			NonNullList<Ingredient> ingredients = NonNullList.create();
			ingredients.add(Ingredient.of(ModItems.BACKPACK.get()));
			ingredients.add(Ingredient.of(color.getTag()));
			recipes.add(new ShapedRecipe(id, "", 1, 2, ingredients, backpackOutput));
		}
	}
}
