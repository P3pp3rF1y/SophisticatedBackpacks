package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.google.gson.JsonObject;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.ShapedRecipe;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistryEntry;

import javax.annotation.Nullable;
import java.util.function.Function;

public class ShapedSerializer<T extends IShapeBasedRecipe> extends ForgeRegistryEntry<IRecipeSerializer<?>> implements IRecipeSerializer<T> {
	private final Function<ShapedRecipe, T> initialize;

	public ShapedSerializer(Function<ShapedRecipe, T> initialize) {
		this.initialize = initialize;
	}

	@Override
	public T read(ResourceLocation recipeId, JsonObject json) {
		return initialize.apply(IRecipeSerializer.CRAFTING_SHAPED.read(recipeId, json));
	}

	@Nullable
	@Override
	public T read(ResourceLocation recipeId, PacketBuffer buffer) {
		ShapedRecipe compose = IRecipeSerializer.CRAFTING_SHAPED.read(recipeId, buffer);
		return compose == null ? null : initialize.apply(compose);
	}

	@Override
	public void write(PacketBuffer buffer, T recipe) {
		IRecipeSerializer.CRAFTING_SHAPED.write(buffer, recipe.getCompose());
	}
}

