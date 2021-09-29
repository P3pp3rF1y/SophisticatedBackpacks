package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;

import java.util.Optional;

public class UpgradeNextTierRecipe extends ShapedRecipe implements IWrapperRecipe<ShapedRecipe> {
	public static final Serializer SERIALIZER = new Serializer();
	private final ShapedRecipe compose;

	public UpgradeNextTierRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(), compose.getResultItem());
		this.compose = compose;
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public ItemStack assemble(CraftingContainer inv) {
		ItemStack nextTier = super.assemble(inv);
		getUpgrade(inv).ifPresent(upgrade -> nextTier.setTag(upgrade.getTag()));
		return nextTier;
	}

	private Optional<ItemStack> getUpgrade(CraftingContainer inv) {
		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.getItem() instanceof IBackpackUpgradeItem) {
				return Optional.of(slotStack);
			}
		}
		return Optional.empty();
	}

	@Override
	public RecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}

	public static class Serializer extends RecipeWrapperSerializer<ShapedRecipe, UpgradeNextTierRecipe> {
		public Serializer() {
			super(UpgradeNextTierRecipe::new, RecipeSerializer.SHAPED_RECIPE);
		}
	}
}
