package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.ClientRecipeHelper;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class BackpackSmithingUpgradeRecipesMaker {
	private BackpackSmithingUpgradeRecipesMaker() {
	}

	public static List<BackpackSmithingUpgradeDisplayRecipe> getGroupedSmithingRecipes() {
		return ClientRecipeHelper.transformAllRecipesOfTypeIntoMultiple(RecipeType.SMITHING, SmithingBackpackUpgradeRecipe.class, recipe -> {
			BackpackSmithingUpgradeDisplayRecipe displayRecipe = createDisplayRecipe(recipe);
			return List.of(displayRecipe);
		});
	}

	private static BackpackSmithingUpgradeDisplayRecipe createDisplayRecipe(SmithingBackpackUpgradeRecipe recipe) {
		Map<String, BackpackTierUpgradeVariantPair> variantPairs = new LinkedHashMap<>();
		for (ItemStack source : getBackpackItems(recipe)) {
			ItemStack result = ClientRecipeHelper.getResultItem(recipe).copy();
			copyColors(source, result);
			variantPairs.putIfAbsent(source.getItem() + String.valueOf(source.getTag()), new BackpackTierUpgradeVariantPair(source.copy(), result));
		}
		ResourceLocation id = recipe.getId().withPath(path -> "backpack_smithing_upgrade_grouped/" + path);
		return new BackpackSmithingUpgradeDisplayRecipe(id, recipe, recipe.getTemplateIngredient(), recipe.getAdditionIngredient(), List.copyOf(variantPairs.values()));
	}

	private static List<ItemStack> getBackpackItems(SmithingBackpackUpgradeRecipe recipe) {
		List<ItemStack> backpackItems = new ArrayList<>();
		for (ItemStack ingredientItem : recipe.getBaseIngredient().getItems()) {
			Item item = ingredientItem.getItem();
			if (item instanceof BackpackItem) {
				addRecipeViewerVariants(backpackItems, item);
			}
		}
		return backpackItems;
	}

	private static void addRecipeViewerVariants(List<ItemStack> backpackItems, Item item) {
		backpackItems.add(new ItemStack(item));
		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(item);
			int colorValue = net.p3pp3rf1y.sophisticatedcore.util.ColorHelper.getColor(color.getTextureDiffuseColors());
			BackpackItem.setColors(stack, colorValue, colorValue);
			backpackItems.add(stack);
		}
		ItemStack stack = new ItemStack(item);
		BackpackItem.setColors(stack, net.p3pp3rf1y.sophisticatedcore.util.ColorHelper.getColor(DyeColor.YELLOW.getTextureDiffuseColors()), net.p3pp3rf1y.sophisticatedcore.util.ColorHelper.getColor(DyeColor.LIME.getTextureDiffuseColors()));
		backpackItems.add(stack);
	}

	private static void copyColors(ItemStack from, ItemStack to) {
		BackpackItem.setColors(to, BackpackItem.getMainColor(from), BackpackItem.getAccentColor(from));
	}
}
