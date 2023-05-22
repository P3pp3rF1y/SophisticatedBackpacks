package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.SimpleRecipeSerializer;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.crafting.StorageDyeRecipeBase;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;

import java.util.List;

public class BackpackDyeRecipe extends StorageDyeRecipeBase {
	public static final SimpleRecipeSerializer<BackpackDyeRecipe> SERIALIZER = new SimpleRecipeSerializer<>(BackpackDyeRecipe::new);
	public BackpackDyeRecipe(ResourceLocation registryName) {
		super(registryName);
	}

	@Override
	public RecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}

	@Override
	protected boolean isDyeableStorageItem(ItemStack stack) {
		return stack.getItem() instanceof BackpackItem;
	}

	@Override
	protected void applyColors(ItemStack coloredStorage, List<DyeColor> mainDyes, List<DyeColor> trimDyes) {
		coloredStorage.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(coloredWrapper -> coloredWrapper.setColors(ColorHelper.calculateColor(coloredWrapper.getMainColor(), BackpackWrapper.DEFAULT_CLOTH_COLOR, mainDyes),
						ColorHelper.calculateColor(coloredWrapper.getAccentColor(), BackpackWrapper.DEFAULT_BORDER_COLOR, trimDyes)
				));
	}
}
