package net.p3pp3rf1y.sophisticatedcore.crafting;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CustomRecipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.SimpleRecipeSerializer;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;

public class UpgradeClearRecipe extends CustomRecipe {
	public static final SimpleRecipeSerializer<UpgradeClearRecipe> SERIALIZER = new SimpleRecipeSerializer<>(UpgradeClearRecipe::new);

	public UpgradeClearRecipe(ResourceLocation registryName) {
		super(registryName);
	}

	@Override
	public boolean matches(CraftingContainer inventory, Level level) {
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

		return upgradePresent;
	}

	@Override
	public ItemStack assemble(CraftingContainer inventory) {
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
	public RecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}
}
