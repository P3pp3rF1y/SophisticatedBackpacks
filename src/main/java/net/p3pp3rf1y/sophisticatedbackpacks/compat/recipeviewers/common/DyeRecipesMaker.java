package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeDisplayGenerator;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;

import java.util.List;

public class DyeRecipesMaker {
	private DyeRecipesMaker() {
	}

	public static <R> void addRecipes(IRecipeDisplayGenerator<R> generator) {
		addSingleColorRecipes(generator);
		addMultipleColorsRecipe(generator);
	}

	private static <R> void addMultipleColorsRecipe(IRecipeDisplayGenerator<R> generator) {
		int clothColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_MAIN_COLOR, List.of(
				DyeColor.YELLOW, DyeColor.LIME
		));
		int trimColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_ACCENT_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR, List.of(
				DyeColor.BLUE, DyeColor.BLACK
		));
		ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
		BackpackWrapper.fromStack(backpackOutput).setColors(clothColor, trimColor);

		generator.shaped(backpackOutput)
				.pattern("YB ")
				.pattern("LEA")
				.define('Y', DyeColor.YELLOW.getTag())
				.define('B', ModItems.BACKPACK.get())
				.define('L', DyeColor.LIME.getTag())
				.define('E', DyeColor.BLUE.getTag())
				.define('A', DyeColor.BLACK.getTag())
				.save(ResourceKey.create(Registries.RECIPE, ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "multiple_colors")));
	}

	private static <R> void addSingleColorRecipes(IRecipeDisplayGenerator<R> generator) {
		for (DyeColor color : DyeColor.values()) {
			ItemStack backpackOutput = new ItemStack(ModItems.BACKPACK.get());
			BackpackWrapper.fromStack(backpackOutput).setColors(color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
			generator.shaped(backpackOutput)
					.pattern("D")
					.pattern("B")
					.define('D', color.getTag())
					.define('B', ModItems.BACKPACK.get())
					.save(ResourceKey.create(Registries.RECIPE, ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "single_color_" + color.getSerializedName())));
		}
	}
}
