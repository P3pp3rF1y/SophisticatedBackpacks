package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.ShapedRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgrade;

import java.util.Optional;

public class UpgradeNextTierRecipe extends ShapedRecipe implements IShapeBasedRecipe {
	public static final Serializer SERIALIZER = new Serializer();
	private final ShapedRecipe compose;

	public UpgradeNextTierRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(), compose.getRecipeOutput());
		this.compose = compose;
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public ItemStack getCraftingResult(CraftingInventory inv) {
		ItemStack nextTier = super.getCraftingResult(inv);
		getUpgrade(inv).ifPresent(upgrade -> nextTier.setTag(upgrade.getTag()));
		return nextTier;
	}

	private Optional<ItemStack> getUpgrade(CraftingInventory inv) {
		for (int slot = 0; slot < inv.getSizeInventory(); slot++) {
			ItemStack slotStack = inv.getStackInSlot(slot);
			if (slotStack.getItem() instanceof IBackpackUpgrade) {
				return Optional.of(slotStack);
			}
		}
		return Optional.empty();
	}

	@Override
	public IRecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}

	public static class Serializer extends ShapedSerializer<UpgradeNextTierRecipe> {
		public Serializer() {
			super(UpgradeNextTierRecipe::new);
		}
	}
}
