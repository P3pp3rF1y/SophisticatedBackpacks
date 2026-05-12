package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.core.HolderLookup;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.SmithingRecipeInput;
import net.minecraft.world.item.crafting.SmithingTransformRecipe;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.IWrapperRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.RecipeWrapperSerializer;

import java.util.Optional;

public class SmithingBackpackUpgradeRecipe extends SmithingTransformRecipe implements IWrapperRecipe<SmithingTransformRecipe> {
	private final SmithingTransformRecipe compose;

	public SmithingBackpackUpgradeRecipe(SmithingTransformRecipe compose) {
		super(compose.template, compose.base, compose.addition, compose.result);
		this.compose = compose;
	}

	@Override
	public boolean isSpecial() {
		return true;
	}

	@Override
	public ItemStack assemble(SmithingRecipeInput inv, HolderLookup.Provider registryAccess) {
		ItemStack upgradedBackpack = result.copy();
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			getBackpack(inv).map(ItemStack::getComponents).ifPresent(upgradedBackpack::applyComponents);
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(upgradedBackpack);
			BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
			wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots());
		}
		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(SmithingRecipeInput inv) {
		ItemStack slotStack = inv.getItem(1);
		if (slotStack.getItem() instanceof BackpackItem) {
			return Optional.of(slotStack);
		}
		return Optional.empty();
	}

	@Override
	public RecipeSerializer<?> getSerializer() {
		return ModItems.SMITHING_BACKPACK_UPGRADE_RECIPE_SERIALIZER.get();
	}

	@Override
	public SmithingTransformRecipe getCompose() {
		return compose;
	}

	public Ingredient getTemplateIngredient() {
		return compose.template;
	}

	public Ingredient getBaseIngredient() {
		return compose.base;
	}

	public Ingredient getAdditionIngredient() {
		return compose.addition;
	}

	public static class Serializer extends RecipeWrapperSerializer<SmithingTransformRecipe, SmithingBackpackUpgradeRecipe> {
		public Serializer() {
			super(SmithingBackpackUpgradeRecipe::new, RecipeSerializer.SMITHING_TRANSFORM);
		}
	}
}
