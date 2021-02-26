package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.google.common.collect.ImmutableMap;
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
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import java.util.Map;
import java.util.function.Predicate;

public class BackpackTwoDyesRecipe extends SpecialRecipe {
	public static final SpecialRecipeSerializer<BackpackTwoDyesRecipe> SERIALIZER = new SpecialRecipeSerializer<>(BackpackTwoDyesRecipe::new);
	private static final int RECIPE_WIDTH = 3;
	private static final int RECIPE_HEIGHT = 1;
	private static final Map<Integer, Predicate<Item>> ingredientItemMatchers = ImmutableMap.of(
			0, i -> i.isIn(Tags.Items.DYES),
			1, i -> i instanceof BackpackItem,
			2, i -> i.isIn(Tags.Items.DYES)
	);

	public BackpackTwoDyesRecipe(ResourceLocation registryName) {
		super(registryName);
	}

	@Override
	public boolean matches(CraftingInventory inv, World worldIn) {
		for (int i = 0; i <= inv.getWidth() - RECIPE_WIDTH; ++i) {
			for (int j = 0; j <= inv.getHeight() - RECIPE_HEIGHT; ++j) {
				if (checkMatch(inv, i, j)) {
					return true;
				}
			}
		}

		return false;
	}

	private boolean checkMatch(CraftingInventory craftingInventory, int width, int height) {
		for (int i = 0; i < craftingInventory.getWidth(); ++i) {
			for (int j = 0; j < craftingInventory.getHeight(); ++j) {
				ItemStack slotStack = craftingInventory.getStackInSlot(i + j * craftingInventory.getWidth());
				Item item = slotStack.getItem();
				int k = i - width;
				int l = j - height;
				if (k >= 0 && l >= 0 && k < RECIPE_WIDTH && l < RECIPE_HEIGHT) {
					if (!ingredientItemMatchers.get(k).test(item)) {
						return false;
					}
				} else if (!slotStack.isEmpty()) {
					return false;
				}
			}
		}

		return true;
	}

	@Override
	public ItemStack getCraftingResult(CraftingInventory inv) {
		ItemStack dyeOne = ItemStack.EMPTY;
		ItemStack backpack = ItemStack.EMPTY;
		ItemStack dyeTwo = ItemStack.EMPTY;
		for (int slot = 0; slot < inv.getSizeInventory(); slot++) {
			ItemStack slotStack = inv.getStackInSlot(slot);
			Item item = slotStack.getItem();
			if (item instanceof BackpackItem) {
				backpack = slotStack;
			} else if (item.isIn(Tags.Items.DYES)) {
				if (dyeOne.isEmpty()) {
					dyeOne = slotStack;
				} else {
					dyeTwo = slotStack;
				}
			}
		}
		if (dyeOne.isEmpty() || dyeTwo.isEmpty() || backpack.isEmpty()) {
			return ItemStack.EMPTY;
		}

		ItemStack coloredBackpack = backpack.copy();
		DyeColor colorOne = DyeColor.getColor(dyeOne);
		DyeColor colorTwo = DyeColor.getColor(dyeTwo);
		if (colorOne == null || colorTwo == null) {
			return ItemStack.EMPTY;
		}
		coloredBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(coloredWrapper -> coloredWrapper.setColors(colorOne.getColorValue(), colorTwo.getColorValue()));

		return coloredBackpack;
	}

	@Override
	public boolean canFit(int width, int height) {
		return width >= 3 && height >= 1;
	}

	@Override
	public IRecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}
}
