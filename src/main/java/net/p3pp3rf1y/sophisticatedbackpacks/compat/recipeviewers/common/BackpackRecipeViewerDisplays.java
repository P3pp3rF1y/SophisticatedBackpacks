package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.ClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayCatalog;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayContext;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SingleColorDyeRecipeSpec;

public class BackpackRecipeViewerDisplays {
	private BackpackRecipeViewerDisplays() {
	}

	public static void register(IRecipeViewerDisplayCatalog catalog, IRecipeViewerDisplayContext context) {
		registerDyeRecipes(catalog, context);
		catalog.addCraftingSpecExtensionRecipeClass(BackpackUpgradeRecipe.class);
		BackpackTierUpgradeRecipesMaker.getGroupedShapedCraftingRecipes(context::getSubtypeInterpreter).stream().map(BackpackTierUpgradeDisplayRecipe::toSpec)
				.forEach(catalog::addCraftingSpec);
		BackpackSmithingUpgradeRecipesMaker.getGroupedSmithingRecipes().stream().map(BackpackSmithingUpgradeDisplayRecipe::toSpec)
				.forEach(catalog::addSmithingSpec);
		ClientRecipeHelper
				.transformAllRecipeHoldersOfType(RecipeType.CRAFTING, BackpackUpgradeRecipe.class,
						(id, recipeHolder) -> new RecipeHolder<CraftingRecipe>(ClientRecipeHelper.recipeKey(id), recipeHolder.value()))
				.forEach(catalog::addCraftingRecipe);
	}

	public static void registerDyeRecipes(IRecipeViewerDisplayCatalog catalog, IRecipeViewerDisplayContext context) {
		DyeRecipesMaker.getSingleColorRecipeSpecs().stream()
				.map(spec -> new SingleColorDyeRecipeSpec(spec.id(), spec.sourceStacks(), spec.variantPairs(),
						(recipeResult, focusedOutput) -> context.getSubtypeInterpreter(focusedOutput)
								.map(interpreter -> recipeResult.is(focusedOutput.getItem())
										&& interpreter.getComparableData(recipeResult).equals(interpreter.getComparableData(focusedOutput)))
								.orElse(ItemStack.isSameItemSameComponents(recipeResult, focusedOutput))))
				.forEach(catalog::addGroupedCraftingSpec);
		DyeRecipesMaker.getMultipleColorsRecipes().forEach(catalog::addCraftingRecipe);
	}

	public static boolean needsSyntheticSmithingDisplay(ItemStack stack) {
		if (!(stack.getItem() instanceof BackpackItem)) {
			return false;
		}
		BackpackWrapper wrapper = (BackpackWrapper) BackpackWrapper.fromStack(stack);
		return wrapper.getMainColor() != BackpackWrapper.DEFAULT_MAIN_COLOR || wrapper.getAccentColor() != BackpackWrapper.DEFAULT_ACCENT_COLOR;
	}
}
