package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.StorageDyeRecipeBase;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;

import java.util.List;

public class BackpackDyeRecipe extends StorageDyeRecipeBase {
	public BackpackDyeRecipe(CraftingBookCategory category) {
		super(category);
	}

	@Override
	public RecipeSerializer<BackpackDyeRecipe> getSerializer() {
		return ModItems.BACKPACK_DYE_RECIPE_SERIALIZER.get();
	}

	@Override
	protected boolean isDyeableStorageItem(ItemStack stack) {
		return stack.getItem() instanceof BackpackItem;
	}

	@Override
	protected void applyColors(ItemStack coloredStorage, List<DyeColor> mainDyes, List<DyeColor> trimDyes) {
		IBackpackWrapper coloredWrapper = BackpackWrapper.fromStack(coloredStorage);
		coloredWrapper.setColors(ColorHelper.calculateColor(coloredWrapper.getMainColor(), BackpackWrapper.DEFAULT_MAIN_COLOR, mainDyes),
				ColorHelper.calculateColor(coloredWrapper.getAccentColor(), BackpackWrapper.DEFAULT_ACCENT_COLOR, trimDyes));
	}
}
