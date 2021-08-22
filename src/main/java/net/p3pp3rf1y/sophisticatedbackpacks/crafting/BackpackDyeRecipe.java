package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.DyeColor;
import net.minecraft.item.DyeItem;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.SpecialRecipe;
import net.minecraft.item.crafting.SpecialRecipeSerializer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.Tuple;
import net.minecraft.world.World;
import net.minecraftforge.common.Tags;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BackpackDyeRecipe extends SpecialRecipe {
	public static final SpecialRecipeSerializer<BackpackDyeRecipe> SERIALIZER = new SpecialRecipeSerializer<>(BackpackDyeRecipe::new);

	public BackpackDyeRecipe(ResourceLocation registryName) {
		super(registryName);
	}

	@Override
	public boolean matches(CraftingInventory inv, World worldIn) {
		boolean backpackPresent = false;
		boolean dyePresent = false;
		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.isEmpty()) {
				continue;
			}
			Item item = slotStack.getItem();
			if (item instanceof BackpackItem) {
				if (backpackPresent) {
					return false;
				}
				backpackPresent = true;
			} else if (item instanceof DyeItem) {
				dyePresent = true;
			} else {
				return false;
			}
		}
		return backpackPresent && dyePresent;
	}

	@Override
	public ItemStack assemble(CraftingInventory inv) {
		Map<Integer, List<DyeColor>> columnDyes = new HashMap<>();
		Tuple<Integer, ItemStack> columnBackpack = null;

		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.isEmpty()) {
				continue;
			}
			Item item = slotStack.getItem();
			int column = slot % inv.getWidth();
			if (item instanceof BackpackItem) {
				if (columnBackpack != null) {
					return ItemStack.EMPTY;
				}

				columnBackpack = new Tuple<>(column, slotStack);
			} else if (item.is(Tags.Items.DYES)) {
				DyeColor dyeColor = DyeColor.getColor(slotStack);
				if (dyeColor == null) {
					return ItemStack.EMPTY;
				}
				columnDyes.computeIfAbsent(column, c -> new ArrayList<>()).add(dyeColor);
			} else {
				return ItemStack.EMPTY;
			}
		}
		if (columnBackpack == null) {
			return ItemStack.EMPTY;
		}

		ItemStack coloredBackpack = columnBackpack.getB().copy();
		int backpackColumn = columnBackpack.getA();

		applyBackpackColors(columnDyes, coloredBackpack, backpackColumn);

		return coloredBackpack;
	}

	private void applyBackpackColors(Map<Integer, List<DyeColor>> columnDyes, ItemStack coloredBackpack, int backpackColumn) {
		List<DyeColor> clothDyes = new ArrayList<>();
		List<DyeColor> trimDyes = new ArrayList<>();

		for (Map.Entry<Integer, List<DyeColor>> entry : columnDyes.entrySet()) {
			if (entry.getKey() <= backpackColumn) {
				clothDyes.addAll(entry.getValue());
			}
			if (entry.getKey() >= backpackColumn) {
				trimDyes.addAll(entry.getValue());
			}
		}

		coloredBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(coloredWrapper -> coloredWrapper.setColors(calculateColor(coloredWrapper.getClothColor(), BackpackWrapper.DEFAULT_CLOTH_COLOR, clothDyes),
						calculateColor(coloredWrapper.getBorderColor(), BackpackWrapper.DEFAULT_BORDER_COLOR, trimDyes)
				));
	}

	public static int calculateColor(int baseColor, int defaultColor, List<DyeColor> dyes) {
		if (dyes.isEmpty()) {
			return baseColor;
		}

		int[] rgb = new int[3];
		int sumMaxComponent = 0;
		int numberOfColors = 0;
		if (baseColor != defaultColor) {
			float baseRed = (baseColor >> 16 & 255);
			float baseGreen = (baseColor >> 8 & 255);
			float baseBlue = (baseColor & 255);
			sumMaxComponent = (int) ((float) sumMaxComponent + Math.max(baseRed, Math.max(baseGreen, baseBlue)));
			rgb[0] = (int) ((float) rgb[0] + baseRed);
			rgb[1] = (int) ((float) rgb[1] + baseGreen);
			rgb[2] = (int) ((float) rgb[2] + baseBlue);
			++numberOfColors;
		}

		for (DyeColor dye : dyes) {
			float[] dyeRgb = dye.getTextureDiffuseColors();
			int dyeRed = (int) (dyeRgb[0] * 255.0F);
			int dyeGreen = (int) (dyeRgb[1] * 255.0F);
			int dyeBlue = (int) (dyeRgb[2] * 255.0F);
			sumMaxComponent += Math.max(dyeRed, Math.max(dyeGreen, dyeBlue));
			rgb[0] += dyeRed;
			rgb[1] += dyeGreen;
			rgb[2] += dyeBlue;
			++numberOfColors;
		}

		int avgRed = rgb[0] / numberOfColors;
		int avgGreen = rgb[1] / numberOfColors;
		int avgBlue = rgb[2] / numberOfColors;
		float avgMaxComponent = (float) sumMaxComponent / (float) numberOfColors;
		float maxAvgComponent = (float) Math.max(avgRed, Math.max(avgGreen, avgBlue));
		avgRed = (int) ((float) avgRed * avgMaxComponent / maxAvgComponent);
		avgGreen = (int) ((float) avgGreen * avgMaxComponent / maxAvgComponent);
		avgBlue = (int) ((float) avgBlue * avgMaxComponent / maxAvgComponent);
		int finalColor = (avgRed << 8) + avgGreen;
		finalColor = (finalColor << 8) + avgBlue;

		return finalColor;
	}

	@Override
	public boolean canCraftInDimensions(int width, int height) {
		return width >= 2 && height >= 1;
	}

	@Override
	public IRecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}
}
