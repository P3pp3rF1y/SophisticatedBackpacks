package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.DyeColor;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.SpecialRecipe;
import net.minecraft.item.crafting.SpecialRecipeSerializer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.common.Tags;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

public class BackpackSingleDyeRecipe extends SpecialRecipe {
	public static final SpecialRecipeSerializer<BackpackSingleDyeRecipe> SERIALIZER = new SpecialRecipeSerializer<>(BackpackSingleDyeRecipe::new);

	public BackpackSingleDyeRecipe(ResourceLocation registryName) {
		super(registryName);
	}

	@Override
	public boolean matches(CraftingInventory inv, World worldIn) {
		int ingredientCount = 0;

		for (int j = 0; j < inv.getSizeInventory(); ++j) {
			ItemStack itemstack = inv.getStackInSlot(j);
			if (!itemstack.isEmpty()
					&& (++ingredientCount > 2 || !(itemstack.getItem().isIn(Tags.Items.DYES) || itemstack.getItem() instanceof BackpackItem))) {
				return false;
			}
		}

		return ingredientCount == 2;
	}

	@Override
	public ItemStack getCraftingResult(CraftingInventory inv) {
		ItemStack dye = ItemStack.EMPTY;
		ItemStack backpack = ItemStack.EMPTY;
		for (int slot = 0; slot < inv.getSizeInventory(); slot++) {
			ItemStack slotStack = inv.getStackInSlot(slot);
			Item item = slotStack.getItem();
			if (item instanceof BackpackItem) {
				backpack = slotStack;
			} else if (item.isIn(Tags.Items.DYES)) {
				dye = slotStack;
			}
		}
		if (dye.isEmpty() || backpack.isEmpty()) {
			return ItemStack.EMPTY;
		}

		ItemStack coloredBackpack = new ItemStack(backpack.getItem());
		BackpackWrapper coloredWrapper = new BackpackWrapper(coloredBackpack, true);
		new BackpackWrapper(backpack, false).copyDataTo(coloredWrapper);
		DyeColor dyeColor = DyeColor.getColor(dye);
		if (dyeColor == null) {
			return ItemStack.EMPTY;
		}
		int color = dyeColor.getColorValue();
		coloredWrapper.setColors(color, color);
		return coloredBackpack;
	}

	@Override
	public boolean canFit(int width, int height) {
		return width * height >= 2;
	}

	@Override
	public IRecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}
}
