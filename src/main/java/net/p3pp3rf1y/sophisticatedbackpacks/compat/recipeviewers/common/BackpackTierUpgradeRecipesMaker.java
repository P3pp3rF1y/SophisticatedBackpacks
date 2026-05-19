package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.core.NonNullList;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.inventory.TransientCraftingContainer;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.*;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.ClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public class BackpackTierUpgradeRecipesMaker {
	private BackpackTierUpgradeRecipesMaker() {
	}

	public static <T extends PropertyBasedSubtypeInterpreter> List<BackpackTierUpgradeDisplayRecipe> getGroupedShapedCraftingRecipes(Function<ItemStack, Optional<T>> subtypeInterpreterGetter) {
		return ClientRecipeHelper.transformAllRecipeHoldersOfTypeIntoMultiple(RecipeType.CRAFTING, BackpackUpgradeRecipe.class, recipeHolder -> {
			BackpackTierUpgradeDisplayRecipe displayRecipe = createDisplayRecipe(recipeHolder, subtypeInterpreterGetter);
			return List.of(displayRecipe);
		});
	}

	private static <T extends CraftingRecipe, U extends PropertyBasedSubtypeInterpreter> BackpackTierUpgradeDisplayRecipe createDisplayRecipe(RecipeHolder<T> recipeHolder,
			Function<ItemStack, Optional<U>> getSubtypeInterpreter) {
		T recipe = recipeHolder.value();
		CraftingContainer craftingInventory = createCraftingInventory();
		int backpackIngredientIndex = findBackpackIngredientIndex(recipe.getIngredients());
		NonNullList<Ingredient> ingredientsCopy = copyIngredients(recipe.getIngredients());
		Map<String, BackpackTierUpgradeVariantPair> variantPairs = new LinkedHashMap<>();
		for (ItemStack backpackItem : getBackpackItems(recipe)) {
			populateCraftingInventory(recipe.getIngredients(), craftingInventory, backpackIngredientIndex, backpackItem);
			ItemStack result = ClientRecipeHelper.assemble(recipe, craftingInventory.asCraftInput());
			BackpackTierUpgradeVariantPair pair = new BackpackTierUpgradeVariantPair(backpackItem.copy(), result.copy());
			variantPairs.putIfAbsent(getPairKey(pair, getSubtypeInterpreter), pair);
		}
		ResourceLocation id = recipeHolder.id().withPath(path -> "backpack_tier_upgrade_grouped/" + path);
		int width = recipe instanceof ShapedRecipe shapedRecipe ? shapedRecipe.getWidth() : 0;
		int height = recipe instanceof ShapedRecipe shapedRecipe ? shapedRecipe.getHeight() : 0;
		RecipeHolder<CraftingRecipe> displayRecipeHolder = new RecipeHolder<>(recipeHolder.id(), new ShapedRecipe("", CraftingBookCategory.MISC,
				new ShapedRecipePattern(width, height, ingredientsCopy, Optional.empty()), ClientRecipeHelper.getResultItem(recipe)));
		return new BackpackTierUpgradeDisplayRecipe(id, displayRecipeHolder, width, height, ingredientsCopy, backpackIngredientIndex, List.copyOf(variantPairs.values()));
	}

	private static CraftingContainer createCraftingInventory() {
		return new TransientCraftingContainer(new AbstractContainerMenu(null, -1) {
			@Override
			public ItemStack quickMoveStack(Player player, int index) {
				return ItemStack.EMPTY;
			}

			public boolean stillValid(Player playerIn) {
				return false;
			}
		}, 3, 3);
	}

	private static NonNullList<Ingredient> copyIngredients(NonNullList<Ingredient> ingredients) {
		NonNullList<Ingredient> ingredientsCopy = NonNullList.createWithCapacity(ingredients.size());
		ingredientsCopy.addAll(ingredients);
		return ingredientsCopy;
	}

	private static int findBackpackIngredientIndex(NonNullList<Ingredient> ingredients) {
		for (int i = 0; i < ingredients.size(); i++) {
			for (ItemStack ingredientItem : ingredients.get(i).getItems()) {
				if (ingredientItem.getItem() instanceof BackpackItem) {
					return i;
				}
			}
		}
		throw new IllegalStateException("Backpack tier upgrade recipe missing backpack ingredient");
	}

	private static void populateCraftingInventory(NonNullList<Ingredient> ingredients, CraftingContainer craftingInventory, int backpackIngredientIndex, ItemStack backpackItem) {
		for (int i = 0; i < ingredients.size(); i++) {
			if (i == backpackIngredientIndex) {
				craftingInventory.setItem(i, backpackItem.copy());
				continue;
			}
			Ingredient ingredient = ingredients.get(i);
			ItemStack[] ingredientItems = ingredient.getItems();
			craftingInventory.setItem(i, ingredient.isEmpty() ? ItemStack.EMPTY : ingredientItems[0]);
		}
	}

	private static <U extends PropertyBasedSubtypeInterpreter> String getPairKey(BackpackTierUpgradeVariantPair pair, Function<ItemStack, Optional<U>> getSubtypeInterpreter) {
		return getSubtypeInterpreter.apply(pair.source()).map(interpreter -> interpreter.getRegistrySanitizedItemString(pair.source())).orElse(pair.source().toString())
				+ "->"
				+ getSubtypeInterpreter.apply(pair.result()).map(interpreter -> interpreter.getRegistrySanitizedItemString(pair.result())).orElse(pair.result().toString());
	}

	private static List<ItemStack> getBackpackItems(CraftingRecipe recipe) {
		NonNullList<ItemStack> backpackItems = NonNullList.create();
		for (Ingredient ingredient : recipe.getIngredients()) {
			ItemStack[] ingredientItems = ingredient.getItems();

			for (ItemStack ingredientItem : ingredientItems) {
				Item item = ingredientItem.getItem();
				if (item instanceof BackpackItem) {
					addRecipeViewerVariants(backpackItems, item);
				}
			}
		}

		return backpackItems;
	}

	private static void addRecipeViewerVariants(List<ItemStack> backpackItems, Item item) {
		backpackItems.add(new ItemStack(item));
		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(item);
			BackpackItem.setColors(stack, color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
			backpackItems.add(stack);
		}
		ItemStack stack = new ItemStack(item);
		BackpackItem.setColors(stack, DyeColor.YELLOW.getTextureDiffuseColor(), DyeColor.LIME.getTextureDiffuseColor());
		backpackItems.add(stack);
	}
}
