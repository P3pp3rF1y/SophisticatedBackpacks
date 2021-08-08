package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.ShapedRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import java.util.Optional;

public class BackpackUpgradeRecipe extends ShapedRecipe implements IWrapperRecipe<ShapedRecipe> {
	public static final Serializer SERIALIZER = new Serializer();
	private final ShapedRecipe compose;

	public BackpackUpgradeRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(), compose.getResultItem());
		this.compose = compose;
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public ItemStack assemble(CraftingInventory inv) {
		ItemStack upgradedBackpack = super.assemble(inv);
		getBackpack(inv).flatMap(backpack -> Optional.ofNullable(backpack.getTag())).ifPresent(tag -> upgradedBackpack.setTag(tag.copy()));
		upgradedBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
			BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
			wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots());
		});

		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(CraftingInventory inv) {
		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
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

	public static class Serializer extends RecipeWrapperSerializer<ShapedRecipe, BackpackUpgradeRecipe> {
		public Serializer() {
			super(BackpackUpgradeRecipe::new, IRecipeSerializer.SHAPED_RECIPE);
		}
	}
}
