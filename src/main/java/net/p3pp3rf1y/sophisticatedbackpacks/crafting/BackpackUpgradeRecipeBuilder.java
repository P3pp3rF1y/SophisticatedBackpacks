package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import net.minecraft.data.IFinishedRecipe;
import net.minecraft.item.Item;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.Ingredient;
import net.minecraft.tags.ITag;
import net.minecraft.util.IItemProvider;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

public class BackpackUpgradeRecipeBuilder {
	private final Item itemResult;
	private final List<String> pattern = Lists.newArrayList();
	private final Map<Character, Ingredient> keyIngredients = Maps.newLinkedHashMap();

	public BackpackUpgradeRecipeBuilder(IItemProvider itemResult) {
		this.itemResult = itemResult.asItem();
	}

	public static BackpackUpgradeRecipeBuilder shapedRecipe(IItemProvider itemResult) {
		return new BackpackUpgradeRecipeBuilder(itemResult);
	}

	public BackpackUpgradeRecipeBuilder key(Character symbol, ITag<Item> tagIn) {
		return key(symbol, Ingredient.fromTag(tagIn));
	}

	public BackpackUpgradeRecipeBuilder key(Character symbol, IItemProvider itemIn) {
		return key(symbol, Ingredient.fromItems(itemIn));
	}

	public BackpackUpgradeRecipeBuilder key(Character symbol, Ingredient ingredientIn) {
		if (keyIngredients.containsKey(symbol)) {
			throw new IllegalArgumentException("Symbol '" + symbol + "' is already defined!");
		} else if (symbol == ' ') {
			throw new IllegalArgumentException("Symbol ' ' (whitespace) is reserved and cannot be defined");
		} else {
			keyIngredients.put(symbol, ingredientIn);
			return this;
		}
	}

	public BackpackUpgradeRecipeBuilder patternLine(String patternIn) {
		if (!pattern.isEmpty() && patternIn.length() != pattern.get(0).length()) {
			throw new IllegalArgumentException("Pattern must be the same width on every line!");
		} else {
			pattern.add(patternIn);
			return this;
		}
	}

	public void build(Consumer<IFinishedRecipe> consumerIn) {
		build(consumerIn, Registry.ITEM.getKey(itemResult));
	}

	public void build(Consumer<IFinishedRecipe> consumerIn, String save) {
		ResourceLocation resourcelocation = Registry.ITEM.getKey(itemResult);
		if ((new ResourceLocation(save)).equals(resourcelocation)) {
			throw new IllegalStateException("Backpack Upgrade Recipe " + save + " should remove its 'save' argument");
		} else {
			build(consumerIn, new ResourceLocation(save));
		}
	}

	public void build(Consumer<IFinishedRecipe> consumerIn, ResourceLocation id) {
		validate(id);
		consumerIn.accept(new Result(id, itemResult, pattern, keyIngredients));
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
		private final Item itemResult;
		private final List<String> pattern;
		private final Map<Character, Ingredient> key;

		public Result(ResourceLocation id, Item itemResult, List<String> pattern, Map<Character, Ingredient> keyIngredients) {
			this.id = id;
			this.itemResult = itemResult;
			this.pattern = pattern;
			key = keyIngredients;
		}

		public void serialize(JsonObject json) {
			JsonArray jsonarray = new JsonArray();

			for (String s : pattern) {
				jsonarray.add(s);
			}

			json.add("pattern", jsonarray);
			JsonObject jsonobject = new JsonObject();

			for (Map.Entry<Character, Ingredient> entry : key.entrySet()) {
				jsonobject.add(String.valueOf(entry.getKey()), entry.getValue().serialize());
			}

			json.add("key", jsonobject);
			JsonObject jsonobject1 = new JsonObject();
			jsonobject1.addProperty("item", Registry.ITEM.getKey(itemResult).toString());

			json.add("result", jsonobject1);
		}

		public IRecipeSerializer<?> getSerializer() {
			return BackpackUpgradeRecipe.SERIALIZER;
		}

		public ResourceLocation getID() {
			return id;
		}

		@Nullable
		public JsonObject getAdvancementJson() {
			return null;
		}

		@Nullable
		public ResourceLocation getAdvancementID() {
			return null;
		}
	}
}
