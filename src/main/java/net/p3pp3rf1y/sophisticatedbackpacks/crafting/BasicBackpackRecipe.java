package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.IWrapperRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.RecipeWrapperSerializer;

public class BasicBackpackRecipe extends ShapedRecipe implements IWrapperRecipe<ShapedRecipe> {
	private final ShapedRecipe compose;

	public BasicBackpackRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(), compose.getResultItem());
		this.compose = compose;
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public ItemStack assemble(CraftingContainer inv) {
		ItemStack result = super.assemble(inv);
		removeUuid(result);
		return result;
	}

	private void removeUuid(ItemStack backpack) {
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(IBackpackWrapper::removeContentsUuid);
	}

	@Override
	public RecipeSerializer<?> getSerializer() {
		return ModItems.BASIC_BACKPACK_RECIPE_SERIALIZER.get();
	}

	public static class Serializer extends RecipeWrapperSerializer<ShapedRecipe, BasicBackpackRecipe> {
		public Serializer() {
			super(BasicBackpackRecipe::new, RecipeSerializer.SHAPED_RECIPE);
		}
	}
}
