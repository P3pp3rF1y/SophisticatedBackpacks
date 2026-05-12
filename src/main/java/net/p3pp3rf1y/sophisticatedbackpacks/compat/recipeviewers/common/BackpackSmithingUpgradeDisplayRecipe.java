package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.SmithingRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SmithingDisplaySpec;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SmithingDisplayVariant;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SmithingSourceResultFocusBehavior;

import java.util.List;
import java.util.Optional;

public record BackpackSmithingUpgradeDisplayRecipe(ResourceLocation id, RecipeHolder<SmithingRecipe> recipeHolder, Ingredient template, Ingredient addition,
												  List<BackpackTierUpgradeVariantPair> variantPairs) {

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
		return new BackpackTierUpgradeVariantPair(resultStack.transmuteCopy(pair.source().getItem(), 1), resultStack.copy());
	}

	public SmithingDisplaySpec toSpec() {
		List<SmithingDisplayVariant> displayVariants = variantPairs.stream().map(pair -> new SmithingDisplayVariant(pair.source(), pair.result())).toList();
		List<SmithingDisplayVariant> globalVariants = variantPairs.stream()
				.filter(pair -> isUntinted(pair.source()) && isUntinted(pair.result()))
				.map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()))
				.toList();
		return new SmithingDisplaySpec(id, Optional.of(template), Optional.of(addition), displayVariants, globalVariants,
				new SmithingSourceResultFocusBehavior(this::focusSource, this::focusResult));
	}

	private static boolean isUntinted(ItemStack stack) {
		BackpackWrapper wrapper = (BackpackWrapper) BackpackWrapper.fromStack(stack);
		return wrapper.getMainColor() == BackpackWrapper.DEFAULT_MAIN_COLOR && wrapper.getAccentColor() == BackpackWrapper.DEFAULT_ACCENT_COLOR;
	}

	private Optional<SmithingDisplayVariant> focusSource(SmithingDisplayVariant variant, ItemStack focusedInput) {
		Optional<BackpackTierUpgradeVariantPair> exactPair = findBySource(focusedInput);
		if (exactPair.isPresent()) {
			return exactPair.filter(pair -> ItemStack.isSameItemSameComponents(variant.base(), pair.source())).map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
		}
		return findBySourceItem(focusedInput)
				.filter(pair -> ItemStack.isSameItemSameComponents(variant.base(), pair.source()))
				.map(pair -> withComponentsFromSource(pair, focusedInput))
				.map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
	}

	private Optional<SmithingDisplayVariant> focusResult(SmithingDisplayVariant variant, ItemStack focusedOutput) {
		Optional<BackpackTierUpgradeVariantPair> exactPair = findByResult(focusedOutput);
		if (exactPair.isPresent()) {
			return exactPair.filter(pair -> ItemStack.isSameItemSameComponents(variant.result(), pair.result())).map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
		}
		return findByResultItem(focusedOutput)
				.filter(pair -> ItemStack.isSameItemSameComponents(variant.result(), pair.result()))
				.map(pair -> withComponentsFromResult(pair, focusedOutput))
				.map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
	}
}
