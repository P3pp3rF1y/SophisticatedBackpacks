package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.core.NonNullList;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.CraftingDisplaySpec;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.CraftingDisplayVariant;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SourceResultFocusBehavior;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public record BackpackTierUpgradeDisplayRecipe(ResourceLocation id, RecipeHolder<CraftingRecipe> recipeHolder, int width, int height,
											   NonNullList<Ingredient> ingredients, int backpackIngredientIndex, List<BackpackTierUpgradeVariantPair> variantPairs) {
	public Optional<BackpackTierUpgradeVariantPair> findBySource(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItemSameComponents(pair.source(), stack)).findFirst();
	}

	public Optional<BackpackTierUpgradeVariantPair> findBySourceItem(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItem(pair.source(), stack)).findFirst();
	}

	public Optional<BackpackTierUpgradeVariantPair> findByResult(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItemSameComponents(pair.result(), stack)).findFirst();
	}

	public Optional<BackpackTierUpgradeVariantPair> findByResultItem(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItem(pair.result(), stack)).findFirst();
	}

	private static BackpackTierUpgradeVariantPair withComponentsFromSource(BackpackTierUpgradeVariantPair pair, ItemStack sourceStack) {
		return new BackpackTierUpgradeVariantPair(sourceStack.copy(), sourceStack.transmuteCopy(pair.result().getItem(), 1));
	}

	private static BackpackTierUpgradeVariantPair withComponentsFromResult(BackpackTierUpgradeVariantPair pair, ItemStack resultStack) {
		if (resultStack.getComponentsPatch().isEmpty()) {
			return pair;
		}
		return new BackpackTierUpgradeVariantPair(resultStack.transmuteCopy(pair.source().getItem(), 1), resultStack.copy());
	}

	public CraftingDisplaySpec toSpec() {
		List<CraftingDisplayVariant> displayVariants = variantPairs.stream().map(this::toVariant).toList();
		List<CraftingDisplayVariant> globalVariants = variantPairs.stream()
				.filter(pair -> isUntinted(pair.source()) && isUntinted(pair.result()))
				.map(this::toVariant)
				.toList();
		return new CraftingDisplaySpec(id, false, width, height, ingredients, displayVariants, globalVariants, Set.of(recipeHolder.id().location()),
				new SourceResultFocusBehavior(backpackIngredientIndex, this::focusSource, this::focusResult));
	}

	private static boolean isUntinted(ItemStack stack) {
		BackpackWrapper wrapper = (BackpackWrapper) BackpackWrapper.fromStack(stack);
		return wrapper.getMainColor() == BackpackWrapper.DEFAULT_MAIN_COLOR && wrapper.getAccentColor() == BackpackWrapper.DEFAULT_ACCENT_COLOR;
	}

	private CraftingDisplayVariant toVariant(BackpackTierUpgradeVariantPair pair) {
		List<ItemStack> inputs = new ArrayList<>(ingredients.size());
		for (int i = 0; i < ingredients.size(); i++) {
			inputs.add(i == backpackIngredientIndex ? pair.source() : ItemStack.EMPTY);
		}
		return new CraftingDisplayVariant(inputs, List.of(pair.result()));
	}

	private Optional<CraftingDisplayVariant> focusSource(CraftingDisplayVariant variant, ItemStack focusedInput) {
		if (isUntinted(focusedInput)) {
			return Optional.empty();
		}
		ItemStack source = getSource(variant);
		Optional<BackpackTierUpgradeVariantPair> exactPair = findBySource(focusedInput);
		if (exactPair.isPresent()) {
			return exactPair.filter(pair -> ItemStack.isSameItemSameComponents(source, pair.source())).map(this::toVariant);
		}
		return findBySourceItem(focusedInput)
				.filter(pair -> ItemStack.isSameItemSameComponents(source, pair.source()))
				.map(pair -> withComponentsFromSource(pair, focusedInput))
				.map(this::toVariant);
	}

	private Optional<CraftingDisplayVariant> focusResult(CraftingDisplayVariant variant, ItemStack focusedOutput) {
		if (isUntinted(focusedOutput)) {
			return Optional.empty();
		}
		Optional<BackpackTierUpgradeVariantPair> exactPair = findByResult(focusedOutput);
		if (exactPair.isPresent()) {
			return exactPair.filter(pair -> ItemStack.isSameItemSameComponents(variant.firstOutput(), pair.result())).map(this::toVariant);
		}
		return findByResultItem(focusedOutput)
				.filter(pair -> ItemStack.isSameItemSameComponents(variant.firstOutput(), pair.result()))
				.map(pair -> withComponentsFromResult(pair, focusedOutput))
				.map(this::toVariant);
	}

	private static ItemStack getSource(CraftingDisplayVariant variant) {
		return variant.inputs().stream().filter(stack -> !stack.isEmpty()).findFirst().orElse(ItemStack.EMPTY);
	}
}
