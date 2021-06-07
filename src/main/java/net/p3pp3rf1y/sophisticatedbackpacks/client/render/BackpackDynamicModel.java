package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.datafixers.util.Either;
import com.mojang.datafixers.util.Pair;
import net.minecraft.block.BlockState;
import net.minecraft.client.renderer.model.BakedQuad;
import net.minecraft.client.renderer.model.BlockModel;
import net.minecraft.client.renderer.model.IBakedModel;
import net.minecraft.client.renderer.model.IModelTransform;
import net.minecraft.client.renderer.model.IUnbakedModel;
import net.minecraft.client.renderer.model.ItemCameraTransforms;
import net.minecraft.client.renderer.model.ItemOverrideList;
import net.minecraft.client.renderer.model.ModelBakery;
import net.minecraft.client.renderer.model.RenderMaterial;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.world.ClientWorld;
import net.minecraft.entity.LivingEntity;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.item.ItemStack;
import net.minecraft.resources.IResourceManager;
import net.minecraft.util.Direction;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.vector.Quaternion;
import net.minecraft.util.math.vector.TransformationMatrix;
import net.minecraft.util.math.vector.Vector3f;
import net.minecraftforge.client.model.IModelConfiguration;
import net.minecraftforge.client.model.IModelLoader;
import net.minecraftforge.client.model.data.IDynamicBakedModel;
import net.minecraftforge.client.model.data.IModelData;
import net.minecraftforge.client.model.geometry.IModelGeometry;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;

public class BackpackDynamicModel implements IModelGeometry<BackpackDynamicModel> {
	private final Map<ModelPart, IUnbakedModel> modelParts;

	public BackpackDynamicModel(Map<ModelPart, IUnbakedModel> modelParts) {
		this.modelParts = modelParts;
	}

	@Override
	public IBakedModel bake(IModelConfiguration owner, ModelBakery bakery, Function<RenderMaterial, TextureAtlasSprite> spriteGetter, IModelTransform modelTransform, ItemOverrideList overrides, ResourceLocation modelLocation) {
		ImmutableMap.Builder<ModelPart, IBakedModel> builder = ImmutableMap.builder();
		modelParts.forEach((part, model) -> {
			IBakedModel bakedModel = model.bakeModel(bakery, spriteGetter, modelTransform, modelLocation);
			if (bakedModel != null) {
				builder.put(part, bakedModel);
			}
		});
		return new BakedModel(builder.build());
	}

	@Override
	public Collection<RenderMaterial> getTextures(IModelConfiguration owner, Function<ResourceLocation, IUnbakedModel> modelGetter, Set<Pair<String, String>> missingTextureErrors) {
		ImmutableSet.Builder<RenderMaterial> builder = ImmutableSet.builder();
		modelParts.forEach((part, model) -> builder.addAll(model.getTextures(modelGetter, missingTextureErrors)));
		return builder.build();
	}

	private static final class BakedModel implements IDynamicBakedModel {
		private static final Map<ItemCameraTransforms.TransformType, TransformationMatrix> TRANSFORMS;

		static {
			ImmutableMap.Builder<ItemCameraTransforms.TransformType, TransformationMatrix> builder = ImmutableMap.builder();
			builder.put(ItemCameraTransforms.TransformType.THIRD_PERSON_LEFT_HAND, new TransformationMatrix(
					new Vector3f(0, -2 / 16f, -4.5f / 16f),
					new Quaternion(85, -90, 0, true),
					new Vector3f(0.75f, 0.75f, 0.75f), null
			));
			builder.put(ItemCameraTransforms.TransformType.THIRD_PERSON_RIGHT_HAND, new TransformationMatrix(
					new Vector3f(0, -2 / 16f, -4.5f / 16f),
					new Quaternion(85, -90, 0, true),
					new Vector3f(0.75f, 0.75f, 0.75f), null
			));
			builder.put(ItemCameraTransforms.TransformType.FIRST_PERSON_LEFT_HAND, new TransformationMatrix(
					new Vector3f(0, 0, 0),
					new Quaternion(0, 0, 0, true),
					new Vector3f(0.5f, 0.5f, 0.5f), null
			));
			builder.put(ItemCameraTransforms.TransformType.FIRST_PERSON_RIGHT_HAND, new TransformationMatrix(
					new Vector3f(0, 0, 0),
					new Quaternion(0, 0, 0, true),
					new Vector3f(0.5f, 0.5f, 0.5f), null
			));
			builder.put(ItemCameraTransforms.TransformType.HEAD, new TransformationMatrix(
					new Vector3f(0, 14.25f / 16f, 0),
					new Quaternion(0, 0, 0, true),
					new Vector3f(1, 1, 1), null
			));
			builder.put(ItemCameraTransforms.TransformType.GUI, new TransformationMatrix(
					new Vector3f(0, 1.25f / 16f, 0),
					new Quaternion(30, 225, 0, true),
					new Vector3f(0.9f, 0.9f, 0.9f), null
			));
			builder.put(ItemCameraTransforms.TransformType.GROUND, new TransformationMatrix(
					new Vector3f(0, 3 / 16f, 0),
					new Quaternion(0, 0, 0, true),
					new Vector3f(0.5f, 0.5f, 0.5f), null
			));
			builder.put(ItemCameraTransforms.TransformType.FIXED, new TransformationMatrix(
					new Vector3f(0, 0, -2.25f / 16f),
					new Quaternion(0, 0, 0, true),
					new Vector3f(0.75f, 0.75f, 0.75f), null
			));

			TRANSFORMS = builder.build();
		}

		private final BackpackItemOverrideList overrideList = new BackpackItemOverrideList(this);
		private final Map<ModelPart, IBakedModel> models;

		public BakedModel(Map<ModelPart, IBakedModel> models) {
			this.models = models;
		}

		@Nonnull
		@Override
		public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, @Nonnull Random rand, @Nonnull IModelData extraData) {
			List<BakedQuad> ret = new ArrayList<>();

			ret.addAll(models.get(ModelPart.BASE).getQuads(state, side, rand, extraData));
			ret.addAll(models.get(ModelPart.LEFT_POUCH).getQuads(state, side, rand, extraData));
			ret.addAll(models.get(ModelPart.RIGHT_POUCH).getQuads(state, side, rand, extraData));
			ret.addAll(models.get(ModelPart.FRONT_POUCH).getQuads(state, side, rand, extraData));

			return ret;
		}

		@Override
		public boolean isAmbientOcclusion() {
			return true;
		}

		@Override
		public boolean isGui3d() {
			return true;
		}

		@Override
		public boolean isSideLit() {
			return true;
		}

		@Override
		public boolean isBuiltInRenderer() {
			return false;
		}

		@SuppressWarnings("java:S1874") //don't have model data to pass in here and just calling getParticleTexture of baked model that doesn't need model data
		@Override
		public TextureAtlasSprite getParticleTexture() {
			//noinspection deprecation
			return models.get(ModelPart.BASE).getParticleTexture();
		}

		@Override
		public ItemOverrideList getOverrides() {
			return overrideList;
		}

		@Override
		public IBakedModel handlePerspective(ItemCameraTransforms.TransformType cameraTransformType, MatrixStack matrixStack) {
			TransformationMatrix tr = TRANSFORMS.get(cameraTransformType);

			if (!tr.isIdentity()) {
				tr.push(matrixStack);
			}
			return this;
		}
	}

	private static class BackpackItemOverrideList extends ItemOverrideList {
		private final BackpackDynamicModel.BakedModel backpackModel;

		public BackpackItemOverrideList(BackpackDynamicModel.BakedModel backpackModel) {
			this.backpackModel = backpackModel;
		}

		@Nullable
		@Override
		public IBakedModel getOverrideModel(IBakedModel model, ItemStack stack, @Nullable ClientWorld world, @Nullable LivingEntity livingEntity) {
			return backpackModel;
		}
	}

	public static final class Loader implements IModelLoader<BackpackDynamicModel> {
		public static final Loader INSTANCE = new Loader();

		@Override
		public void onResourceManagerReload(IResourceManager resourceManager) {
			//noop
		}

		@Override
		public BackpackDynamicModel read(JsonDeserializationContext deserializationContext, JsonObject modelContents) {
			ImmutableMap.Builder<ModelPart, IUnbakedModel> builder = ImmutableMap.builder();

			ImmutableMap.Builder<String, Either<RenderMaterial, String>> texturesBuilder = ImmutableMap.builder();
			if (modelContents.has("clipsTexture")) {
				ResourceLocation clipsTexture = ResourceLocation.tryCreate(modelContents.get("clipsTexture").getAsString());
				if (clipsTexture != null) {
					texturesBuilder.put("clips", Either.left(new RenderMaterial(PlayerContainer.LOCATION_BLOCKS_TEXTURE, clipsTexture)));
				}
			}
			ImmutableMap<String, Either<RenderMaterial, String>> textures = texturesBuilder.build();
			for (ModelPart part : ModelPart.values()) {
				addPartModel(builder, part, textures);
			}
			return new BackpackDynamicModel(builder.build());
		}

		private void addPartModel(ImmutableMap.Builder<ModelPart, IUnbakedModel> builder, ModelPart modelPart, ImmutableMap<String, Either<RenderMaterial, String>> textures) {
			builder.put(modelPart, new BlockModel(RegistryHelper.getRL("block/backpack_" + modelPart.name().toLowerCase()), Collections.emptyList(), textures, true, null, ItemCameraTransforms.DEFAULT, Collections.emptyList()));
		}
	}

	private enum ModelPart {
		BASE,
		BATTERY,
		FRONT_POUCH,
		LEFT_POUCH,
		LEFT_TANK,
		RIGHT_POUCH,
		RIGHT_TANK
	}
}
