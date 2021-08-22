package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.ICriterionInstance;
import net.minecraft.advancements.IRequirementsStrategy;
import net.minecraft.advancements.criterion.RecipeUnlockedTrigger;
import net.minecraft.data.IFinishedRecipe;
import net.minecraft.item.Item;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.Ingredient;
import net.minecraft.tags.ITag;
import net.minecraft.util.IItemProvider;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.crafting.CraftingHelper;
import net.minecraftforge.common.crafting.conditions.ICondition;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

public class ShapeBasedRecipeBuilder {
	private final Item itemResult;
	private final List<ICondition> conditions = new ArrayList<>();
	private final List<String> pattern = new ArrayList<>();
	private final Map<Character, Ingredient> keyIngredients = Maps.newLinkedHashMap();
	private final IRecipeSerializer<?> serializer;
	private final Advancement.Builder advancementBuilder = Advancement.Builder.advancement();

	public ShapeBasedRecipeBuilder(IItemProvider itemResult, IRecipeSerializer<?> serializer) {
		this.itemResult = itemResult.asItem();
		this.serializer = serializer;
	}

	public static ShapeBasedRecipeBuilder shapedRecipe(IItemProvider itemResult) {
		return shapedRecipe(itemResult, IRecipeSerializer.SHAPED_RECIPE);
	}

	public static ShapeBasedRecipeBuilder shapedRecipe(IItemProvider itemResult, IRecipeSerializer<?> serializer) {
		return new ShapeBasedRecipeBuilder(itemResult, serializer);
	}

	public ShapeBasedRecipeBuilder key(Character symbol, ITag<Item> tagIn) {
		return key(symbol, Ingredient.of(tagIn));
	}

	public ShapeBasedRecipeBuilder key(Character symbol, IItemProvider itemIn) {
		return key(symbol, Ingredient.of(itemIn));
	}

	public ShapeBasedRecipeBuilder key(Character symbol, Ingredient ingredientIn) {
		if (keyIngredients.containsKey(symbol)) {
			throw new IllegalArgumentException("Symbol '" + symbol + "' is already defined!");
		} else if (symbol == ' ') {
			throw new IllegalArgumentException("Symbol ' ' (whitespace) is reserved and cannot be defined");
		} else {
			keyIngredients.put(symbol, ingredientIn);
			return this;
		}
	}

	public ShapeBasedRecipeBuilder patternLine(String patternIn) {
		if (!pattern.isEmpty() && patternIn.length() != pattern.get(0).length()) {
			throw new IllegalArgumentException("Pattern must be the same width on every line!");
		} else {
			pattern.add(patternIn);
			return this;
		}
	}

	public ShapeBasedRecipeBuilder addCriterion(String name, ICriterionInstance criterion) {
		advancementBuilder.addCriterion(name, criterion);
		return this;
	}

	public void build(Consumer<IFinishedRecipe> consumerIn) {
		build(consumerIn, RegistryHelper.getItemKey(itemResult));
	}

	public void build(Consumer<IFinishedRecipe> consumerIn, ResourceLocation id) {
		validate(id);
		advancementBuilder.parent(new ResourceLocation("recipes/root")).addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(id)).rewards(AdvancementRewards.Builder.recipe(id)).requirements(IRequirementsStrategy.OR);
		consumerIn.accept(new Result(id, conditions, itemResult, pattern, keyIngredients, advancementBuilder, new ResourceLocation(id.getNamespace(), "recipes/" + getGroup() + "/" + id.getPath()), serializer));
	}

	private String getGroup() {
		return itemResult.getItemCategory() == null ? "" : itemResult.getItemCategory().getRecipeFolderName();
	}

	private void validate(ResourceLocation id) {
		if (pattern.isEmpty()) {
			throw new IllegalStateException("No pattern is defined for shaped recipe " + id + "!");
		}

		Set<Character> set = Sets.newHashSet(keyIngredients.keySet());
		set.remove(' ');

		for (String s : pattern) {
			for (int i = 0; i < s.length(); ++i) {
				char c0 = s.charAt(i);
				if (!keyIngredients.containsKey(c0) && c0 != ' ') {
					throw new IllegalStateException("Pattern in recipe " + id + " uses undefined symbol '" + c0 + "'");
				}

				set.remove(c0);
			}
		}

		if (!set.isEmpty()) {
			throw new IllegalStateException("Ingredients are defined but not used in pattern for recipe " + id);
		} else if (pattern.size() == 1 && pattern.get(0).length() == 1) {
			throw new IllegalStateException("Backpack upgrade recipe " + id + " only takes in a single item - should it be a shapeless recipe instead?");
		}
	}

	public static class Result implements IFinishedRecipe {
		private final ResourceLocation id;
		private final List<ICondition> conditions;
		private final Item itemResult;
		private final List<String> pattern;
		private final Map<Character, Ingredient> key;
		private final ResourceLocation advancementId;
		private final IRecipeSerializer<?> serializer;
		private final Advancement.Builder advancementBuilder;

		@SuppressWarnings("java:S107") //the only way of reducing number of parameters here means adding pretty much unnecessary objec parameter
		public Result(ResourceLocation id, List<ICondition> conditions, Item itemResult, List<String> pattern, Map<Character, Ingredient> keyIngredients, Advancement.Builder advancementBuilder, ResourceLocation advancementId, IRecipeSerializer<?> serializer) {
			this.id = id;
			this.conditions = conditions;
			this.itemResult = itemResult;
			this.pattern = pattern;
			key = keyIngredients;
			this.advancementId = advancementId;
			this.serializer = serializer;
			this.advancementBuilder = advancementBuilder;
			conditions.add(new ItemEnabledCondition(itemResult));
		}

		public void serializeRecipeData(JsonObject json) {
			JsonArray conditionsArray = new JsonArray();
			conditions.forEach(c -> conditionsArray.add(CraftingHelper.serialize(c)));
			json.add("conditions", conditionsArray);

			JsonArray jsonarray = new JsonArray();

			for (String s : pattern) {
				jsonarray.add(s);
			}

			json.add("pattern", jsonarray);
			JsonObject jsonobject = new JsonObject();

			for (Map.Entry<Character, Ingredient> entry : key.entrySet()) {
				jsonobject.add(String.valueOf(entry.getKey()), entry.getValue().toJson());
			}

			json.add("key", jsonobject);
			JsonObject jsonobject1 = new JsonObject();
			jsonobject1.addProperty("item", RegistryHelper.getItemKey(itemResult).toString());

			json.add("result", jsonobject1);
		}

		public IRecipeSerializer<?> getType() {
			return serializer;
		}

		public ResourceLocation getId() {
			return id;
		}

		@Nullable
		public JsonObject serializeAdvancement() {
			return advancementBuilder.serializeToJson();
		}

		@Nullable
		public ResourceLocation getAdvancementId() {
			return advancementId;
		}
	}
}
