package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.ShapedRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

import java.util.Optional;

public class BackpackUpgradeRecipe extends ShapedRecipe implements IShapeBasedRecipe {
	public static final Serializer SERIALIZER = new Serializer();
	private final ShapedRecipe compose;

	public BackpackUpgradeRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(), compose.getRecipeOutput());
		this.compose = compose;
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public ItemStack getCraftingResult(CraftingInventory inv) {
		ItemStack upgradedBackpack = super.getCraftingResult(inv);
		getBackpack(inv).ifPresent(backpack ->
				backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
						.ifPresent(wrapper -> upgradedBackpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
								.ifPresent(wrapper::copyDataTo)));

		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(CraftingInventory inv) {
		for (int slot = 0; slot < inv.getSizeInventory(); slot++) {
			ItemStack slotStack = inv.getStackInSlot(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				return Optional.of(slotStack);
			}
		}

		return Optional.empty();
	}

	@Override
	public IRecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}

	public static class Serializer extends ShapedSerializer<BackpackUpgradeRecipe> {
		public Serializer() {
			super(BackpackUpgradeRecipe::new);
		}
	}
}
