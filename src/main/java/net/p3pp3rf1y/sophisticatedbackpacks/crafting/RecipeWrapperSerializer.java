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
	public R read(ResourceLocation recipeId, JsonObject json) {
		return initialize.apply(recipeSerializer.read(recipeId, json));
	}

	@Nullable
	@Override
	public R read(ResourceLocation recipeId, PacketBuffer buffer) {
		T compose = recipeSerializer.read(recipeId, buffer);
		return compose == null ? null : initialize.apply(compose);
	}

	@Override
	public void write(PacketBuffer buffer, R recipe) {
		recipeSerializer.write(buffer, recipe.getCompose());
	}
}

