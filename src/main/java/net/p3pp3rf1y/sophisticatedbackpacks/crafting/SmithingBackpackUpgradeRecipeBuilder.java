package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.data.recipes.RecipeCategory;
import net.minecraft.data.recipes.RecipeOutput;
import net.minecraft.data.recipes.SmithingTransformRecipeBuilder;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.SmithingTransformRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.HoldingRecipeOutput;

import java.util.function.Function;

public class SmithingBackpackUpgradeRecipeBuilder extends SmithingTransformRecipeBuilder {
	private final Function<SmithingTransformRecipe, ? extends SmithingTransformRecipe> factory;

	public SmithingBackpackUpgradeRecipeBuilder(Function<SmithingTransformRecipe, ? extends SmithingTransformRecipe> factory, Ingredient template,
			Ingredient base, Ingredient addition, Item result) {
		super(template, base, addition, RecipeCategory.MISC, result);
		this.factory = factory;
	}

	@Override
	public void save(RecipeOutput recipeOutput, ResourceLocation id) {
		HoldingRecipeOutput holdingRecipeOutput = new HoldingRecipeOutput(recipeOutput.advancement());
		super.save(holdingRecipeOutput, id);
		if (!(holdingRecipeOutput.getRecipe() instanceof SmithingTransformRecipe compose)) {
			return;
		}

		recipeOutput.accept(id, factory.apply(compose), holdingRecipeOutput.getAdvancementHolder(), holdingRecipeOutput.getConditions());
	}
}
