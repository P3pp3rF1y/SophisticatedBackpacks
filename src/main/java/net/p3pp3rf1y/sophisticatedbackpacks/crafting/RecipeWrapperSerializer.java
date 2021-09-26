package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.google.gson.JsonObject;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraftforge.registries.ForgeRegistryEntry;

import javax.annotation.Nullable;
import java.util.function.Function;

public class RecipeWrapperSerializer<T extends Recipe<?>, R extends Recipe<?> & IWrapperRecipe<T>> extends ForgeRegistryEntry<RecipeSerializer<?>>
		implements RecipeSerializer<R> {
	private final Function<T, R> initialize;
	private final RecipeSerializer<T> recipeSerializer;

	public RecipeWrapperSerializer(Function<T, R> initialize, RecipeSerializer<T> recipeSerializer) {
		this.initialize = initialize;
		this.recipeSerializer = recipeSerializer;
	}

	@Override
	public R fromJson(ResourceLocation recipeId, JsonObject json) {
		return initialize.apply(recipeSerializer.fromJson(recipeId, json));
	}

	@Nullable
	@Override
	public R fromNetwork(ResourceLocation recipeId, FriendlyByteBuf buffer) {
		T compose = recipeSerializer.fromNetwork(recipeId, buffer);
		return compose == null ? null : initialize.apply(compose);
	}

	@Override
	public void toNetwork(FriendlyByteBuf buffer, R recipe) {
		recipeSerializer.toNetwork(buffer, recipe.getCompose());
	}
}

