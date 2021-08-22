package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.google.gson.JsonObject;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistryEntry;

import javax.annotation.Nullable;
import java.util.function.Function;

public class RecipeWrapperSerializer<T extends IRecipe<?>, R extends IRecipe<?> & IWrapperRecipe<T>> extends ForgeRegistryEntry<IRecipeSerializer<?>>
		implements IRecipeSerializer<R> {
	private final Function<T, R> initialize;
	private final IRecipeSerializer<T> recipeSerializer;

	public RecipeWrapperSerializer(Function<T, R> initialize, IRecipeSerializer<T> recipeSerializer) {
		this.initialize = initialize;
		this.recipeSerializer = recipeSerializer;
	}

	@Override
	public R fromJson(ResourceLocation recipeId, JsonObject json) {
		return initialize.apply(recipeSerializer.fromJson(recipeId, json));
	}

	@Nullable
	@Override
	public R fromNetwork(ResourceLocation recipeId, PacketBuffer buffer) {
		T compose = recipeSerializer.fromNetwork(recipeId, buffer);
		return compose == null ? null : initialize.apply(compose);
	}

	@Override
	public void toNetwork(PacketBuffer buffer, R recipe) {
		recipeSerializer.toNetwork(buffer, recipe.getCompose());
	}
}

