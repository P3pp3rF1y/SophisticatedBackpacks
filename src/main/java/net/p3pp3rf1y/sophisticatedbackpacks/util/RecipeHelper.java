package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.FurnaceRecipe;
import net.minecraft.item.crafting.IRecipeType;
import net.minecraft.util.NonNullList;
import net.minecraft.world.World;
import net.minecraftforge.items.ItemStackHandler;
import net.minecraftforge.items.wrapper.RecipeWrapper;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class RecipeHelper {
	private static final Map<Item, CompactingShape> ITEM_COMPACTING_SHAPES = new HashMap<>();
	private static WeakReference<World> world;

	private RecipeHelper() {}

	public static void setWorld(World w) {
		world = new WeakReference<>(w);
	}

	private static Optional<World> getWorld() {
		return Optional.ofNullable(world.get());
	}

	private static void tryCompatingRecipes(Item item) {
		getWorld().ifPresent(w -> {
			if (tryCompactingRecipe(item, w, 2, 2)) {
				RecipeHelper.ITEM_COMPACTING_SHAPES.put(item, CompactingShape.TWO_BY_TWO);
				return;
			}
			if (tryCompactingRecipe(item, w, 3, 3)) {
				RecipeHelper.ITEM_COMPACTING_SHAPES.put(item, CompactingShape.THREE_BY_THREE);
			}
		});
	}

	private static boolean tryCompactingRecipe(Item item, World w, int width, int height) {
		ItemStack result = getCraftingResult(item, w, width, height);
		if (!result.isEmpty()) {
			result = getCraftingResult(result.getItem(), w, 1, 1);
			return (result.getItem() == item || InventoryHelper.anyItemTagMatches(result.getItem(), item)) && result.getCount() == width * height;
		}
		return false;
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

	public static CompactingShape getItemCompactingShape(Item item) {
		initCompactingShapes(item);
		return ITEM_COMPACTING_SHAPES.get(item);
	}

	private static void initCompactingShapes(Item item) {
		if (!ITEM_COMPACTING_SHAPES.containsKey(item)) {
			ITEM_COMPACTING_SHAPES.put(item, CompactingShape.NONE);
			tryCompatingRecipes(item);
		}
	}

	public enum CompactingShape {
		NONE,
		THREE_BY_THREE,
		TWO_BY_TWO
	}
}
