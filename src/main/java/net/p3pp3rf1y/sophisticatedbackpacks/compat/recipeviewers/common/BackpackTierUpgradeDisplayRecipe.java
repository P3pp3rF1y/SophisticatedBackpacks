package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.NonNullList;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.CraftingDisplaySpec;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.CraftingDisplayVariant;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.SourceResultFocusBehavior;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

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
		ItemStack source = pair.source().copy();
		copyRenderComponents(sourceStack, source);
		setSlotNumbers(source);
		ItemStack result = pair.result().copy();
		copyRenderComponents(sourceStack, result);
		setSlotNumbers(result);
		return new BackpackTierUpgradeVariantPair(source, result);
	}

	private static void setSlotNumbers(ItemStack stack) {
		BackpackItem backpackItem = (BackpackItem) stack.getItem();
		BackpackWrapper.fromStack(stack).setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots());
	}

	private static BackpackTierUpgradeVariantPair withComponentsFromResult(BackpackTierUpgradeVariantPair pair, ItemStack resultStack) {
		ItemStack source = pair.source().copy();
		copyRenderComponents(resultStack, source);
		setSlotNumbers(source);
		ItemStack result = pair.result().copy();
		copyRenderComponents(resultStack, result);
		setSlotNumbers(result);
		return new BackpackTierUpgradeVariantPair(source, result);
	}

	private static void copyRenderComponents(ItemStack from, ItemStack to) {
		copyComponent(from, to, ModCoreDataComponents.MAIN_COLOR);
		copyComponent(from, to, ModCoreDataComponents.ACCENT_COLOR);
		copyComponent(from, to, ModCoreDataComponents.RENDER_INFO_TAG);
	}

	private static <T> void copyComponent(ItemStack from, ItemStack to, Supplier<DataComponentType<T>> componentSupplier) {
		DataComponentType<T> component = componentSupplier.get();
		T value = from.get(component);
		if (value != null) {
			to.set(component, value);
		}
	}

	public CraftingDisplaySpec toSpec() {
		List<CraftingDisplayVariant> displayVariants = variantPairs.stream().map(this::toVariant).toList();
		List<CraftingDisplayVariant> globalVariants = variantPairs.stream()
				.filter(pair -> isUntinted(pair.source()) && isUntinted(pair.result()))
				.map(this::toVariant)
				.toList();
		return new CraftingDisplaySpec(id, false, width, height, ingredients, displayVariants, globalVariants, Set.of(recipeHolder.id()),
				new SourceResultFocusBehavior(backpackIngredientIndex, this::focusSource, this::focusResult));
	}

	private static boolean isUntinted(ItemStack stack) {
		return BackpackItem.getMainColor(stack) == BackpackItem.DEFAULT_MAIN_COLOR && BackpackItem.getAccentColor(stack) == BackpackItem.DEFAULT_ACCENT_COLOR;
	}

	private CraftingDisplayVariant toVariant(BackpackTierUpgradeVariantPair pair) {
		List<ItemStack> inputs = new ArrayList<>(ingredients.size());
		for (int i = 0; i < ingredients.size(); i++) {
			inputs.add(i == backpackIngredientIndex ? pair.source() : ItemStack.EMPTY);
		}
		return new CraftingDisplayVariant(inputs, List.of(pair.result()));
	}

	private Optional<CraftingDisplayVariant> focusSource(CraftingDisplayVariant variant, ItemStack focusedInput) {
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
		Optional<BackpackTierUpgradeVariantPair> exactPair = findByResult(focusedOutput);
		if (exactPair.isPresent() && focusedOutput.getComponentsPatch().isEmpty()) {
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
