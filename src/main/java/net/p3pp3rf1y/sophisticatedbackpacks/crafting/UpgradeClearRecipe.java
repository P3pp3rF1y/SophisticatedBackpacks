package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.SpecialRecipe;
import net.minecraft.item.crafting.SpecialRecipeSerializer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

public class UpgradeClearRecipe extends SpecialRecipe {
	public static final SpecialRecipeSerializer<UpgradeClearRecipe> SERIALIZER = new SpecialRecipeSerializer<>(UpgradeClearRecipe::new);

	public UpgradeClearRecipe(ResourceLocation registryName) {
		super(registryName);
	}

	@Override
	public boolean matches(CraftingInventory inventory, World pLevel) {
		boolean upgradePresent = false;
		for (ItemStack stack : inventory.items) {
			if (!stack.isEmpty()) {
				if (stack.getItem() instanceof UpgradeItemBase && stack.hasTag() && !upgradePresent) {
					upgradePresent = true;
				} else {
					return false;
				}
			}
		}

		return true;
	}

	@Override
	public ItemStack assemble(CraftingInventory inventory) {
		ItemStack upgrade = ItemStack.EMPTY;
		for (ItemStack stack : inventory.items) {
			if (!stack.isEmpty() && stack.getItem() instanceof UpgradeItemBase) {
				upgrade = stack;
			}
		}
		ItemStack copy = upgrade.copy();
		copy.setTag(null);
		return copy;
	}

	@Override
	public boolean canCraftInDimensions(int width, int height) {
		return width >= 1 && height >= 1;
	}

	@Override
	public IRecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}
}
