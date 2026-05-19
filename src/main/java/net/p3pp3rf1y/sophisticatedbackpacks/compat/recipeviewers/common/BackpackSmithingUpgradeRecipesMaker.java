package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
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
		return ClientRecipeHelper.transformAllRecipeHoldersOfTypeIntoMultiple(RecipeType.SMITHING, SmithingBackpackUpgradeRecipe.class, recipeHolder -> {
			BackpackSmithingUpgradeDisplayRecipe displayRecipe = createDisplayRecipe(recipeHolder);
			return List.of(displayRecipe);
		});
	}

	private static BackpackSmithingUpgradeDisplayRecipe createDisplayRecipe(RecipeHolder<SmithingBackpackUpgradeRecipe> recipeHolder) {
		SmithingBackpackUpgradeRecipe recipe = recipeHolder.value();
		Map<String, BackpackTierUpgradeVariantPair> variantPairs = new LinkedHashMap<>();
		for (ItemStack source : getBackpackItems(recipe)) {
			ItemStack result = ClientRecipeHelper.getResultItem(recipe).copy();
			copyColors(source, result);
			variantPairs.putIfAbsent(source.getItem() + source.getComponents().toString(), new BackpackTierUpgradeVariantPair(source.copy(), result));
		}
		ResourceLocation id = recipeHolder.id().withPath(path -> "backpack_smithing_upgrade_grouped/" + path);
		return new BackpackSmithingUpgradeDisplayRecipe(id, new RecipeHolder<>(recipeHolder.id(), recipe), recipe.getTemplateIngredient(), recipe.getAdditionIngredient(), List.copyOf(variantPairs.values()));
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
			BackpackItem.setColors(stack, color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
			backpackItems.add(stack);
		}
		ItemStack stack = new ItemStack(item);
		BackpackItem.setColors(stack, DyeColor.YELLOW.getTextureDiffuseColor(), DyeColor.LIME.getTextureDiffuseColor());
		backpackItems.add(stack);
	}

	private static void copyColors(ItemStack from, ItemStack to) {
		BackpackItem.setColors(to, BackpackItem.getMainColor(from), BackpackItem.getAccentColor(from));
	}
}
