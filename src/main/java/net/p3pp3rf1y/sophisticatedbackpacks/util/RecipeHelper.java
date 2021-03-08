package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.FurnaceRecipe;
import net.minecraft.item.crafting.IRecipeType;
import net.minecraft.item.crafting.StonecuttingRecipe;
import net.minecraft.util.NonNullList;
import net.minecraft.world.World;
import net.minecraftforge.items.ItemStackHandler;
import net.minecraftforge.items.wrapper.RecipeWrapper;

import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class RecipeHelper {
	private static final Map<Item, Set<CompactingShape>> ITEM_COMPACTING_SHAPES = new HashMap<>();
	private static WeakReference<World> world;

	private RecipeHelper() {}

	public static void setWorld(World w) {
		world = new WeakReference<>(w);
	}

	private static Optional<World> getWorld() {
		return Optional.ofNullable(world.get());
	}

	private static Set<CompactingShape> getCompactingShapes(Item item) {
		return getWorld().map(w -> {
			Set<CompactingShape> compactingShapes = new HashSet<>();
			ItemStack compactingResult = getCraftingResult(item, w, 2, 2);
			if (!compactingResult.isEmpty()) {
				if (uncompactMatchesItem(compactingResult, w, item, 4)) {
					compactingShapes.add(CompactingShape.TWO_BY_TWO_UNCRAFTABLE);
				} else {
					compactingShapes.add(CompactingShape.TWO_BY_TWO);
				}
			}
			compactingResult = getCraftingResult(item, w, 3, 3);
			if (!compactingResult.isEmpty()) {
				if (uncompactMatchesItem(compactingResult, w, item, 9)) {
					compactingShapes.add(CompactingShape.THREE_BY_THREE_UNCRAFTABLE);
				} else {
					compactingShapes.add(CompactingShape.THREE_BY_THREE);
				}
			}
			return compactingShapes;
		}).orElse(Collections.emptySet());
	}

	private static boolean uncompactMatchesItem(ItemStack result, World w, Item item, int count) {
		result = getCraftingResult(result.getItem(), w, 1, 1);
		return (result.getItem() == item || InventoryHelper.anyItemTagMatches(result.getItem(), item)) && result.getCount() == count;
	}

	public static ItemStack getCraftingResult(Item item, int width, int height) {
		return getWorld().map(w -> getCraftingResult(item, w, width, height)).orElse(ItemStack.EMPTY);
	}

	private static ItemStack getCraftingResult(Item item, World w, int width, int height) {
		CraftingInventory craftingInventory = getFilledCraftingInventory(item, width, height);
		return w.getRecipeManager().getRecipe(IRecipeType.CRAFTING, craftingInventory, w).map(r -> r.getCraftingResult(craftingInventory)).orElse(ItemStack.EMPTY);
	}

	private static CraftingInventory getFilledCraftingInventory(Item item, int width, int height) {
		CraftingInventory craftinginventory = new CraftingInventory(new Container(null, -1) {
			public boolean canInteractWith(PlayerEntity playerIn) {
				return false;
			}
		}, width, height);

		for (int i = 0; i < craftinginventory.getSizeInventory(); i++) {
			craftinginventory.setInventorySlotContents(i, new ItemStack(item));
		}
		return craftinginventory;
	}

	public static Optional<FurnaceRecipe> getSmeltingRecipe(ItemStack stack) {
		return getWorld().flatMap(w -> w.getRecipeManager().getRecipe(IRecipeType.SMELTING, new RecipeWrapper(new ItemStackHandler(NonNullList.from(ItemStack.EMPTY, stack))), w));
	}

	public static Set<CompactingShape> getItemCompactingShapes(Item item) {
		return ITEM_COMPACTING_SHAPES.computeIfAbsent(item, RecipeHelper::getCompactingShapes);
	}

	public static List<StonecuttingRecipe> getStonecuttingRecipes(IInventory inventory) {
		return getWorld().map(w -> w.getRecipeManager().getRecipes(IRecipeType.STONECUTTING, inventory, w)).orElse(Collections.emptyList());
	}

	public enum CompactingShape {
		NONE,
		THREE_BY_THREE,
		TWO_BY_TWO,
		THREE_BY_THREE_UNCRAFTABLE,
		TWO_BY_TWO_UNCRAFTABLE
	}
}
