package net.p3pp3rf1y.sophisticatedcore.crafting;

import com.google.common.collect.Lists;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.CriterionTriggerInstance;
import net.minecraft.advancements.RequirementsStrategy;
import net.minecraft.advancements.critereon.RecipeUnlockedTrigger;
import net.minecraft.data.recipes.FinishedRecipe;
import net.minecraft.data.recipes.RecipeBuilder;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.ItemLike;
import net.minecraftforge.common.crafting.CraftingHelper;
import net.minecraftforge.common.crafting.conditions.ICondition;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class ShapelessBasedRecipeBuilder implements RecipeBuilder {
	private final Item result;
	@Nullable
	private final CompoundTag nbt;
	private final int count;
	private final List<Ingredient> ingredients = Lists.newArrayList();
	private final Advancement.Builder advancement = Advancement.Builder.advancement();
	@Nullable
	private String group;

	public ShapelessBasedRecipeBuilder(ItemLike result, int count, @Nullable CompoundTag nbt) {
		this.result = result.asItem();
		this.count = count;
		this.nbt = nbt;
	}

	public static ShapelessBasedRecipeBuilder shapeless(ItemLike result) {
		return shapeless(result, 1);
	}

	public static ShapelessBasedRecipeBuilder shapeless(ItemLike result, int count) {
		return new ShapelessBasedRecipeBuilder(result, count, null);
	}

	public static ShapelessBasedRecipeBuilder shapeless(ItemStack stack) {
		return new ShapelessBasedRecipeBuilder(stack.getItem(), 1, stack.getTag());
	}

	public ShapelessBasedRecipeBuilder requires(TagKey<Item> tag) {
		return requires(Ingredient.of(tag));
	}

	public ShapelessBasedRecipeBuilder requires(ItemLike item) {
		return requires(item, 1);
	}

	public ShapelessBasedRecipeBuilder requires(ItemLike item, int quantity) {
		for (int i = 0; i < quantity; ++i) {
			requires(Ingredient.of(item));
		}

		return this;
	}

	public ShapelessBasedRecipeBuilder requires(Ingredient ingredient) {
		return requires(ingredient, 1);
	}

	public ShapelessBasedRecipeBuilder requires(Ingredient ingredient, int quantity) {
		for (int i = 0; i < quantity; ++i) {
			ingredients.add(ingredient);
		}

		return this;
	}

	public ShapelessBasedRecipeBuilder unlockedBy(String criterionName, CriterionTriggerInstance criterionTrigger) {
		advancement.addCriterion(criterionName, criterionTrigger);
		return this;
	}

	public ShapelessBasedRecipeBuilder group(@Nullable String groupName) {
		group = groupName;
		return this;
	}

	public Item getResult() {
		return result;
	}

	public void save(Consumer<FinishedRecipe> finishedRecipeConsumer, ResourceLocation recipeId) {
		advancement.parent(new ResourceLocation("recipes/root")).addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(recipeId)).rewards(AdvancementRewards.Builder.recipe(recipeId)).requirements(RequirementsStrategy.OR);
		finishedRecipeConsumer.accept(new ShapelessBasedRecipeBuilder.Result(recipeId, result, nbt, count, group == null ? "" : group, ingredients, advancement, new ResourceLocation(recipeId.getNamespace(), "recipes/" + getGroup() + "/" + recipeId.getPath())));
	}

	private String getGroup() {
		return result.getItemCategory() == null ? "" : result.getItemCategory().getRecipeFolderName();
	}

	public static class Result implements FinishedRecipe {
		private final List<ICondition> conditions = new ArrayList<>();
		private final ResourceLocation id;
		private final Item itemResult;
		@Nullable
		private final CompoundTag nbt;
		private final int count;
		private final String group;
		private final List<Ingredient> ingredients;
		private final Advancement.Builder advancement;
		private final ResourceLocation advancementId;

		@SuppressWarnings("java:S107") //the only way of reducing number of parameters here means adding pretty much unnecessary object parameter
		public Result(ResourceLocation id, Item itemResult, @Nullable
				CompoundTag nbt, int count, String group, List<Ingredient> ingredients, Advancement.Builder advancement, ResourceLocation advancementId) {
			this.id = id;
			this.itemResult = itemResult;
			this.nbt = nbt;
			this.count = count;
			this.group = group;
			this.ingredients = ingredients;
			this.advancement = advancement;
			this.advancementId = advancementId;
			conditions.add(new ItemEnabledCondition(this.itemResult));
		}

		public void serializeRecipeData(JsonObject json) {
			if (!group.isEmpty()) {
				json.addProperty("group", group);
			}

			JsonArray conditionsArray = new JsonArray();
			conditions.forEach(c -> conditionsArray.add(CraftingHelper.serialize(c)));
			json.add("conditions", conditionsArray);

			JsonArray jsonarray = new JsonArray();

			for (Ingredient ingredient : ingredients) {
				jsonarray.add(ingredient.toJson());
			}

			json.add("ingredients", jsonarray);
			JsonObject jsonobject = new JsonObject();
			jsonobject.addProperty("item", RegistryHelper.getItemKey(itemResult).toString());
			if (count > 1) {
				jsonobject.addProperty("count", count);
			}
			if (nbt != null) {
				jsonobject.addProperty("nbt", nbt.toString());
			}

			json.add("result", jsonobject);
		}

		public RecipeSerializer<?> getType() {
			return RecipeSerializer.SHAPELESS_RECIPE;
		}

		public ResourceLocation getId() {
			return id;
		}

		@Nullable
		public JsonObject serializeAdvancement() {
			return advancement.serializeToJson();
		}

		@Nullable
		public ResourceLocation getAdvancementId() {
			return advancementId;
		}
	}
}