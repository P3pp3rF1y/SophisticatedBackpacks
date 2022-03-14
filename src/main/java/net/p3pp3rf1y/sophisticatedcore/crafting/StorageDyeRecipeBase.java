package net.p3pp3rf1y.sophisticatedcore.crafting;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Tuple;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.DyeItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CustomRecipe;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.Tags;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class StorageDyeRecipeBase extends CustomRecipe {
	protected StorageDyeRecipeBase(ResourceLocation registryName) {
		super(registryName);
	}

	@Override
	public boolean matches(CraftingContainer inv, Level worldIn) {
		boolean storagePresent = false;
		boolean dyePresent = false;
		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.isEmpty()) {
				continue;
			}
			Item item = slotStack.getItem();
			if (isStorageItem(item)) {
				if (storagePresent) {
					return false;
				}
				storagePresent = true;
			} else if (item instanceof DyeItem) {
				dyePresent = true;
			} else {
				return false;
			}
		}
		return storagePresent && dyePresent;
	}

	@Override
	public ItemStack assemble(CraftingContainer inv) {
		Map<Integer, List<DyeColor>> columnDyes = new HashMap<>();
		Tuple<Integer, ItemStack> columnStorage = null;

		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.isEmpty()) {
				continue;
			}
			Item item = slotStack.getItem();
			int column = slot % inv.getWidth();
			if (isStorageItem(item)) {
				if (columnStorage != null) {
					return ItemStack.EMPTY;
				}

				columnStorage = new Tuple<>(column, slotStack);
			} else if (slotStack.is(Tags.Items.DYES)) {
				DyeColor dyeColor = DyeColor.getColor(slotStack);
				if (dyeColor == null) {
					return ItemStack.EMPTY;
				}
				columnDyes.computeIfAbsent(column, c -> new ArrayList<>()).add(dyeColor);
			} else {
				return ItemStack.EMPTY;
			}
		}
		if (columnStorage == null) {
			return ItemStack.EMPTY;
		}

		ItemStack coloredStorage = columnStorage.getB().copy();
		int storageColumn = columnStorage.getA();

		applyTintColors(columnDyes, coloredStorage, storageColumn);

		return coloredStorage;
	}

	protected abstract boolean isStorageItem(Item item);

	private void applyTintColors(Map<Integer, List<DyeColor>> columnDyes, ItemStack coloredStorage, int storageColumn) {
		List<DyeColor> mainDyes = new ArrayList<>();
		List<DyeColor> trimDyes = new ArrayList<>();

		for (Map.Entry<Integer, List<DyeColor>> entry : columnDyes.entrySet()) {
			if (entry.getKey() <= storageColumn) {
				mainDyes.addAll(entry.getValue());
			}
			if (entry.getKey() >= storageColumn) {
				trimDyes.addAll(entry.getValue());
			}
		}

		applyColors(coloredStorage, mainDyes, trimDyes);
	}

	protected abstract void applyColors(ItemStack coloredStorage, List<DyeColor> mainDyes, List<DyeColor> trimDyes);

	@Override
	public boolean canCraftInDimensions(int width, int height) {
		return width >= 2 && height >= 1;
	}
}
