package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRequirements;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.Criterion;
import net.minecraft.advancements.criterion.RecipeUnlockedTrigger;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.recipes.RecipeCategory;
import net.minecraft.data.recipes.RecipeOutput;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.Recipe;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public class SmithingBackpackUpgradeRecipeBuilder {
	private final Ingredient template;
	private final Ingredient base;
	private final Ingredient addition;
	private final RecipeCategory category;
	private final Item result;
	private final Map<String, Criterion<?>> criteria = new LinkedHashMap<>();

	public SmithingBackpackUpgradeRecipeBuilder(Ingredient template, Ingredient base, Ingredient addition, Item result) {
		category = RecipeCategory.MISC;
		this.template = template;
		this.base = base;
		this.addition = addition;
		this.result = result;
	}

	public static SmithingBackpackUpgradeRecipeBuilder smithing(Ingredient template, Ingredient base, Ingredient addition, Item result) {
		return new SmithingBackpackUpgradeRecipeBuilder(template, base, addition, result);
	}

	public SmithingBackpackUpgradeRecipeBuilder unlocks(String key, Criterion<?> criterion) {
		criteria.put(key, criterion);
		return this;
	}

	public void save(RecipeOutput recipeOutput, String recipeId) {
		save(recipeOutput, ResourceKey.create(Registries.RECIPE, Identifier.parse(recipeId)));
	}

	public void save(RecipeOutput recipeOutput, ResourceKey<Recipe<?>> id) {
		ensureValid(id);

		Advancement.Builder advancement$builder = recipeOutput.advancement().addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(id)).rewards(AdvancementRewards.Builder.recipe(id)).requirements(AdvancementRequirements.Strategy.OR);
		Objects.requireNonNull(advancement$builder);
		criteria.forEach(advancement$builder::addCriterion);

		recipeOutput.accept(id, new SmithingBackpackUpgradeRecipe(Optional.of(template), base, Optional.of(addition), new ItemStack(result)), advancement$builder.build(id.identifier().withPrefix("recipes/" + category.getFolderName() + "/")));
	}

	private void ensureValid(ResourceKey<Recipe<?>> recipe) {
		if (criteria.isEmpty()) {
			throw new IllegalStateException("No way of obtaining recipe " + recipe.identifier());
		}
	}
}
