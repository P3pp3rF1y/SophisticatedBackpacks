package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.SmithingRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SmithingDisplaySpec;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SmithingDisplayVariant;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SmithingSourceResultFocusBehavior;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public record BackpackSmithingUpgradeDisplayRecipe(ResourceLocation id, SmithingRecipe recipe, Ingredient template, Ingredient addition,
											  List<BackpackTierUpgradeVariantPair> variantPairs) {

	public Optional<BackpackTierUpgradeVariantPair> findBySource(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItemSameTags(pair.source(), stack)).findFirst();
	}

	public Optional<BackpackTierUpgradeVariantPair> findBySourceItem(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItem(pair.source(), stack)).findFirst();
	}

	public Optional<BackpackTierUpgradeVariantPair> findByResult(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItemSameTags(pair.result(), stack)).findFirst();
	}

	public Optional<BackpackTierUpgradeVariantPair> findByResultItem(ItemStack stack) {
		return variantPairs.stream().filter(pair -> ItemStack.isSameItem(pair.result(), stack)).findFirst();
	}

	private static BackpackTierUpgradeVariantPair withComponentsFromSource(BackpackTierUpgradeVariantPair pair, ItemStack sourceStack) {
		ItemStack result = copyWithItem(sourceStack, pair.result().getItem());
		setSlotNumbers(result);
		return new BackpackTierUpgradeVariantPair(sourceStack.copy(), result);
	}

	private static BackpackTierUpgradeVariantPair withComponentsFromResult(BackpackTierUpgradeVariantPair pair, ItemStack resultStack) {
		return new BackpackTierUpgradeVariantPair(copyWithItem(resultStack, pair.source().getItem()), resultStack.copy());
	}

	private static ItemStack copyWithItem(ItemStack stack, Item item) {
		ItemStack copy = new ItemStack(item, stack.getCount());
		copy.setTag(stack.getTag() == null ? null : stack.getTag().copy());
		return copy;
	}

	private static void setSlotNumbers(ItemStack stack) {
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
			BackpackItem backpackItem = (BackpackItem) stack.getItem();
			wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots());
		});
	}

	public SmithingDisplaySpec toSpec() {
		List<SmithingDisplayVariant> displayVariants = variantPairs.stream().map(pair -> new SmithingDisplayVariant(pair.source(), pair.result())).toList();
		List<SmithingDisplayVariant> globalVariants = variantPairs.stream()
				.filter(pair -> isUntinted(pair.source()) && isUntinted(pair.result()))
				.map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()))
				.toList();
		return new SmithingDisplaySpec(id, Optional.of(template), Optional.of(addition), displayVariants, globalVariants, Set.of(recipe),
				new SmithingSourceResultFocusBehavior(this::focusSource, this::focusResult));
	}

	private static boolean isUntinted(ItemStack stack) {
		return BackpackItem.getMainColor(stack) == BackpackItem.DEFAULT_MAIN_COLOR && BackpackItem.getAccentColor(stack) == BackpackItem.DEFAULT_ACCENT_COLOR;
	}

	private Optional<SmithingDisplayVariant> focusSource(SmithingDisplayVariant variant, ItemStack focusedInput) {
		Optional<BackpackTierUpgradeVariantPair> exactPair = findBySource(focusedInput);
		if (exactPair.isPresent()) {
			return exactPair.filter(pair -> ItemStack.isSameItemSameTags(variant.base(), pair.source())).map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
		}
		return findBySourceItem(focusedInput)
				.filter(pair -> ItemStack.isSameItemSameTags(variant.base(), pair.source()))
				.map(pair -> withComponentsFromSource(pair, focusedInput))
				.map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
	}

	private Optional<SmithingDisplayVariant> focusResult(SmithingDisplayVariant variant, ItemStack focusedOutput) {
		Optional<BackpackTierUpgradeVariantPair> exactPair = findByResult(focusedOutput);
		if (exactPair.isPresent()) {
			return exactPair.filter(pair -> ItemStack.isSameItemSameTags(variant.result(), pair.result())).map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
		}
		return findByResultItem(focusedOutput)
				.filter(pair -> ItemStack.isSameItemSameTags(variant.result(), pair.result()))
				.map(pair -> withComponentsFromResult(pair, focusedOutput))
				.map(pair -> new SmithingDisplayVariant(pair.source(), pair.result()));
	}
}
