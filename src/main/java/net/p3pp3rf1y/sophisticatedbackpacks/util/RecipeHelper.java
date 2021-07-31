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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;

import static net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper.CompactingShape.*;

public class RecipeHelper {
	private static final Map<Item, Set<CompactingShape>> ITEM_COMPACTING_SHAPES = new HashMap<>();
	private static final int MAX_FOLLOW_UP_COMPACTING_RECIPES = 10;
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
			getCompactingShape(item, w, 2, 2, TWO_BY_TWO_UNCRAFTABLE, TWO_BY_TWO).ifPresent(compactingShapes::add);
			getCompactingShape(item, w, 3, 3, THREE_BY_THREE_UNCRAFTABLE, THREE_BY_THREE).ifPresent(compactingShapes::add);
			return compactingShapes;
		}).orElse(Collections.emptySet());
	}

	private static Optional<CompactingShape> getCompactingShape(Item item, World w, int width, int height, CompactingShape uncraftableShape, CompactingShape shape) {
		ItemStack compactingResult;
		compactingResult = getCraftingResultAndRemainingItems(item, w, width, height, new ArrayList<>());
		if (!compactingResult.isEmpty()) {
			if (item == compactingResult.getItem()) {
				return Optional.empty();
			}

			if (isPartOfCompactingLoop(item, compactingResult.getItem(), w)) {
				return Optional.empty();
			}

			if (uncompactMatchesItem(compactingResult, w, item, width * height)) {
				return Optional.of(uncraftableShape);
			} else {
				return Optional.of(shape);
			}
		}
		return Optional.empty();
	}

	private static boolean isPartOfCompactingLoop(Item firstCompacted, Item firstCompactResult, World w) {
		ItemStack compactingResult;
		int iterations = 0;
		Set<Item> compactedItems = new HashSet<>();
		Queue<Item> itemsToCompact = new LinkedList<>();
		itemsToCompact.add(firstCompactResult);
		while (!itemsToCompact.isEmpty()) {
			Item itemToCompact = itemsToCompact.poll();
			compactingResult = getCraftingResultAndRemainingItems(itemToCompact, w, 2, 2, new ArrayList<>());
			if (!compactingResult.isEmpty()) {
				if (compactingResult.getItem() == firstCompacted) {
					return true;
				} else if (compactedItems.contains(compactingResult.getItem())) {
					return false; //loop exists but the first compacted item isn't part of it so we will let it be compacted, but no follow up compacting will happen
				}
				itemsToCompact.add(compactingResult.getItem());
			}

			compactingResult = getCraftingResultAndRemainingItems(itemToCompact, w, 3, 3, new ArrayList<>());
			if (!compactingResult.isEmpty()) {
				if (compactingResult.getItem() == firstCompacted) {
					return true;
				} else if (compactedItems.contains(compactingResult.getItem())) {
					return false; //loop exists but the first compacted item isn't part of it so we will let it be compacted, but no follow up compacting will happen
				}
				itemsToCompact.add(compactingResult.getItem());
			}
			compactedItems.add(itemToCompact);
			iterations++;
			if (iterations > MAX_FOLLOW_UP_COMPACTING_RECIPES) {
				return true; //we were unable to figure out if the loop exists because of way too many follow up compacting recipe thus not allowing to compact anyway
			}
		}
		return false;
	}

	private static boolean uncompactMatchesItem(ItemStack result, World w, Item item, int count) {
		result = getCraftingResultAndRemainingItems(result.getItem(), w, 1, 1, new ArrayList<>());
		return (result.getItem() == item || InventoryHelper.anyItemTagMatches(result.getItem(), item)) && result.getCount() == count;
	}

	public static ItemStack getCraftingResultAndRemainingItems(Item item, int width, int height, List<ItemStack> remainingItems) {
		return getWorld().map(w -> getCraftingResultAndRemainingItems(item, w, width, height, remainingItems)).orElse(ItemStack.EMPTY);
	}

	private static ItemStack getCraftingResultAndRemainingItems(Item item, World w, int width, int height, List<ItemStack> remainingItems) {
		CraftingInventory craftingInventory = getFilledCraftingInventory(item, width, height);
		return w.getRecipeManager().getRecipeFor(IRecipeType.CRAFTING, craftingInventory, w).map(r -> {
			r.getRemainingItems(craftingInventory).forEach(stack -> {
				if (!stack.isEmpty()) {
					remainingItems.add(stack);
				}
			});
			return r.assemble(craftingInventory);
		}).orElse(ItemStack.EMPTY);
	}

	private static CraftingInventory getFilledCraftingInventory(Item item, int width, int height) {
		CraftingInventory craftinginventory = new CraftingInventory(new Container(null, -1) {
			public boolean stillValid(PlayerEntity playerIn) {
				return false;
			}
		}, width, height);

		for (int i = 0; i < craftinginventory.getContainerSize(); i++) {
			craftinginventory.setItem(i, new ItemStack(item));
		}
		return craftinginventory;
	}

	public static Optional<FurnaceRecipe> getSmeltingRecipe(ItemStack stack) {
		return getWorld().flatMap(w -> w.getRecipeManager().getRecipeFor(IRecipeType.SMELTING, new RecipeWrapper(new ItemStackHandler(NonNullList.of(ItemStack.EMPTY, stack))), w));
	}

	public static Set<CompactingShape> getItemCompactingShapes(Item item) {
		return ITEM_COMPACTING_SHAPES.computeIfAbsent(item, RecipeHelper::getCompactingShapes);
	}

	public static List<StonecuttingRecipe> getStonecuttingRecipes(IInventory inventory) {
		return getWorld().map(w -> w.getRecipeManager().getRecipesFor(IRecipeType.STONECUTTING, inventory, w)).orElse(Collections.emptyList());
	}

	public enum CompactingShape {
		THREE_BY_THREE,
		TWO_BY_TWO,
		THREE_BY_THREE_UNCRAFTABLE,
		TWO_BY_TWO_UNCRAFTABLE
	}
}
