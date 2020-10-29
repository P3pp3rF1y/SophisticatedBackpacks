package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.google.gson.JsonObject;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.ShapedRecipe;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistryEntry;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

import javax.annotation.Nullable;
import java.util.Optional;

public class BackpackUpgradeRecipe extends ShapedRecipe {
	public static final Serializer SERIALIZER = new Serializer();
	private final ShapedRecipe compose;

	public BackpackUpgradeRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(), compose.getRecipeOutput());
		this.compose = compose;
	}

	@Override
	public ItemStack getCraftingResult(CraftingInventory inv) {
		ItemStack upgradedBackpack = super.getCraftingResult(inv);
		getBackpack(inv).ifPresent(backpack -> new BackpackWrapper(backpack, false).copyDataTo(new BackpackWrapper(upgradedBackpack, true)));

		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(CraftingInventory inv) {
		for(int slot = 0; slot < inv.getSizeInventory(); slot++) {
			ItemStack slotStack = inv.getStackInSlot(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				return Optional.of(slotStack);
			}
		}

		return Optional.empty();
	}

	@Override
	public IRecipeSerializer<?> getSerializer() {
		return SERIALIZER;
	}

	public static class Serializer extends ForgeRegistryEntry<IRecipeSerializer<?>> implements IRecipeSerializer<BackpackUpgradeRecipe> {
		@Override
		public BackpackUpgradeRecipe read(ResourceLocation recipeId, JsonObject json) {
			return new BackpackUpgradeRecipe(IRecipeSerializer.CRAFTING_SHAPED.read(recipeId, json));
		}

		@Nullable
		@Override
		public BackpackUpgradeRecipe read(ResourceLocation recipeId, PacketBuffer buffer) {
			ShapedRecipe compose = IRecipeSerializer.CRAFTING_SHAPED.read(recipeId, buffer);
			return compose == null ? null : new BackpackUpgradeRecipe(compose);
		}

		@Override
		public void write(PacketBuffer buffer, BackpackUpgradeRecipe recipe) {
			IRecipeSerializer.CRAFTING_SHAPED.write(buffer, recipe.compose);
		}
	}
}
