package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.datafixers.util.Either;
import com.mojang.datafixers.util.Pair;
import com.mojang.math.Matrix4f;
import com.mojang.math.Vector3f;
import com.mojang.math.Vector4f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.BlockModel;
import net.minecraft.client.renderer.block.model.ItemOverrides;
import net.minecraft.client.renderer.block.model.ItemTransform;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.Material;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.minecraftforge.client.ChunkRenderTypeSet;
import net.minecraftforge.client.extensions.common.IClientFluidTypeExtensions;
import net.minecraftforge.client.model.IDynamicBakedModel;
import net.minecraftforge.client.model.data.ModelData;
import net.minecraftforge.client.model.geometry.IGeometryBakingContext;
import net.minecraftforge.client.model.geometry.IGeometryLoader;
import net.minecraftforge.client.model.geometry.IUnbakedGeometry;
import net.minecraftforge.client.model.pipeline.QuadBakingVertexConsumer;
import net.minecraftforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;

public class BackpackDynamicModel implements IUnbakedGeometry<BackpackDynamicModel> {
	private final Map<ModelPart, UnbakedModel> modelParts;

	public BackpackDynamicModel(Map<ModelPart, UnbakedModel> modelParts) {
		this.modelParts = modelParts;
	}

	@Override
	public BakedModel bake(IGeometryBakingContext context, ModelBakery bakery, Function<Material, TextureAtlasSprite> spriteGetter, ModelState modelTransform, ItemOverrides overrides, ResourceLocation modelLocation) {
		ImmutableMap.Builder<ModelPart, BakedModel> builder = ImmutableMap.builder();
		modelParts.forEach((part, model) -> {
			BakedModel bakedModel = model.bake(bakery, spriteGetter, modelTransform, modelLocation);
			if (bakedModel != null) {
				builder.put(part, bakedModel);
			}
		});
		return new BackpackBakedModel(builder.build(), modelTransform);
	}

	@Override
	public Collection<Material> getMaterials(IGeometryBakingContext context, Function<ResourceLocation, UnbakedModel> modelGetter, Set<Pair<String, String>> missingTextureErrors) {
		ImmutableSet.Builder<Material> builder = ImmutableSet.builder();
		modelParts.forEach((part, model) -> builder.addAll(model.getMaterials(modelGetter, missingTextureErrors)));
		return builder.build();
	}

	private static final class BackpackBakedModel implements IDynamicBakedModel {
		@Override
		public ChunkRenderTypeSet getRenderTypes(@NotNull BlockState state, @NotNull RandomSource rand, @NotNull ModelData data) {
			return ChunkRenderTypeSet.of(RenderType.cutout());
		}

		public static final Vector3f DEFAULT_ROTATION = new Vector3f(0.0F, 0.0F, 0.0F);
		private static final ItemTransforms ITEM_TRANSFORMS = createItemTransforms();
		private static final ResourceLocation BACKPACK_MODULES_TEXTURE = new ResourceLocation("sophisticatedbackpacks:block/backpack_modules");

		@SuppressWarnings("java:S4738") //ItemTransforms require Guava ImmutableMap to be passed in so no way to change that to java Map
		private static ItemTransforms createItemTransforms() {
			return new ItemTransforms(new ItemTransform(
					new Vector3f(85, -90, 0),
					new Vector3f(0, -2 / 16f, -4.5f / 16f),
					new Vector3f(0.75f, 0.75f, 0.75f), DEFAULT_ROTATION
			), new ItemTransform(
					new Vector3f(85, -90, 0),
					new Vector3f(0, -2 / 16f, -4.5f / 16f),
					new Vector3f(0.75f, 0.75f, 0.75f), DEFAULT_ROTATION
			), new ItemTransform(
					new Vector3f(0, 0, 0),
					new Vector3f(0, 0, 0),
					new Vector3f(0.5f, 0.5f, 0.5f), DEFAULT_ROTATION
			), new ItemTransform(
					new Vector3f(0, 0, 0),
					new Vector3f(0, 0, 0),
					new Vector3f(0.5f, 0.5f, 0.5f), DEFAULT_ROTATION
			), new ItemTransform(
					new Vector3f(0, 0, 0),
					new Vector3f(0, 14.25f / 16f, 0),
					new Vector3f(1, 1, 1), DEFAULT_ROTATION
			), new ItemTransform(
					new Vector3f(30, 225, 0),
					new Vector3f(0, 1.25f / 16f, 0),
					new Vector3f(0.9f, 0.9f, 0.9f), DEFAULT_ROTATION
			), new ItemTransform(
					new Vector3f(0, 0, 0),
					new Vector3f(0, 3 / 16f, 0),
					new Vector3f(0.5f, 0.5f, 0.5f), DEFAULT_ROTATION
			), new ItemTransform(
					new Vector3f(0, 0, 0),
					new Vector3f(0, 0, -2.25f / 16f),
					new Vector3f(0.75f, 0.75f, 0.75f), DEFAULT_ROTATION
			), ImmutableMap.of());
		}

		private final BackpackItemOverrideList overrideList = new BackpackItemOverrideList(this);
		private final Map<ModelPart, BakedModel> models;
		private final ModelState modelTransform;

		private boolean tankLeft;
		@Nullable
		private IRenderedTankUpgrade.TankRenderInfo leftTankRenderInfo = null;
		private boolean tankRight;
		@Nullable
		private IRenderedTankUpgrade.TankRenderInfo rightTankRenderInfo = null;
		private boolean battery;
		@Nullable
		private IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo = null;

		public BackpackBakedModel(Map<ModelPart, BakedModel> models, ModelState modelTransform) {
			this.models = models;
			this.modelTransform = modelTransform;
		}

		@Nonnull
		@Override
		public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData,
				@Nullable RenderType renderType) {
			List<BakedQuad> ret = new ArrayList<>(models.get(ModelPart.BASE).getQuads(state, side, rand, extraData, renderType));
			if (state == null) {
				addLeftSide(state, side, rand, extraData, ret, tankLeft, renderType);
				addRightSide(state, side, rand, extraData, ret, tankRight, renderType);
				addFront(state, side, rand, extraData, ret, battery, renderType);
			} else {
				addLeftSide(state, side, rand, extraData, ret, state.getValue(LEFT_TANK), renderType);
				addRightSide(state, side, rand, extraData, ret, state.getValue(RIGHT_TANK), renderType);
				addFront(state, side, rand, extraData, ret, state.getValue(BATTERY), renderType);
			}

			return ret;
		}

		private void addFront(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, List<BakedQuad> ret,
				boolean battery, @Nullable RenderType renderType) {
			if (battery) {
				if (batteryRenderInfo != null) {
					addCharge(ret, batteryRenderInfo.getChargeRatio());
				}
				ret.addAll(models.get(ModelPart.BATTERY).getQuads(state, side, rand, extraData, renderType));
			} else {
				ret.addAll(models.get(ModelPart.FRONT_POUCH).getQuads(state, side, rand, extraData, renderType));
			}
		}

		private void addCharge(List<BakedQuad> ret, float chargeRatio) {
			if (Mth.equal(chargeRatio, 0)) {
				return;
			}
			int pixels = (int) (chargeRatio * 4);
			float minX = (10 - pixels) / 16f;
			float minY = 2 / 16f;
			float minZ = 1.95f / 16f;
			float maxX = minX + pixels / 16f;
			float maxY = minY + 1 / 16f;
			float[] cols = new float[] {1f, 1f, 1f, 1f};
			TextureAtlasSprite sprite = Minecraft.getInstance().getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(BACKPACK_MODULES_TEXTURE);
			ret.add(createQuad(List.of(getVector(maxX, maxY, minZ), getVector(maxX, minY, minZ), getVector(minX, minY, minZ), getVector(minX, maxY, minZ)), cols, sprite, Direction.NORTH, 14, 14 + (pixels / 2f), 6, 6.5f));
		}

		private void addRightSide(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, List<BakedQuad> ret,
				boolean tankRight, @Nullable RenderType renderType) {
			if (tankRight) {
				if (rightTankRenderInfo != null) {
					rightTankRenderInfo.getFluid().ifPresent(fluid -> addFluid(ret, fluid, rightTankRenderInfo.getFillRatio(), 0.6 / 16d));
				}
				ret.addAll(models.get(ModelPart.RIGHT_TANK).getQuads(state, side, rand, extraData, renderType));
			} else {
				ret.addAll(models.get(ModelPart.RIGHT_POUCH).getQuads(state, side, rand, extraData, renderType));
			}
		}

		private void addLeftSide(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, List<BakedQuad> ret,
				boolean tankLeft, @Nullable RenderType renderType) {
			if (tankLeft) {
				if (leftTankRenderInfo != null) {
					leftTankRenderInfo.getFluid().ifPresent(fluid -> addFluid(ret, fluid, leftTankRenderInfo.getFillRatio(), 12.85 / 16d));
				}
				ret.addAll(models.get(ModelPart.LEFT_TANK).getQuads(state, side, rand, extraData, renderType));
			} else {
				ret.addAll(models.get(ModelPart.LEFT_POUCH).getQuads(state, side, rand, extraData, renderType));
			}
		}

		private void addFluid(List<BakedQuad> ret, FluidStack fluidStack, float ratio, double xMin) {
			if (Mth.equal(ratio, 0.0f)) {
				return;
			}

			double yMin = 1.5 / 16d;
			double yMax = yMin + (ratio * 6) / 16d;
			AABB bounds = new AABB(xMin, yMin, 6.75 / 16d, xMin + 2.5 / 16d, yMax, 9.25 / 16d);

			IClientFluidTypeExtensions renderProperties = IClientFluidTypeExtensions.of(fluidStack.getFluid());
			ResourceLocation texture = renderProperties.getStillTexture(fluidStack);
			int color = renderProperties.getTintColor(fluidStack);
			float[] cols = new float[] {(color >> 24 & 0xFF) / 255F, (color >> 16 & 0xFF) / 255F, (color >> 8 & 0xFF) / 255F, (color & 0xFF) / 255F};
			TextureAtlasSprite still = Minecraft.getInstance().getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(texture);
			float bx1 = 0;
			float bx2 = 5;
			float by1 = 0;
			float by2 = ratio * 10;
			float bz1 = 0;
			float bz2 = 5;

			ret.add(createQuad(List.of(getVector(bounds.minX, bounds.maxY, bounds.minZ), getVector(bounds.minX, bounds.maxY, bounds.maxZ), getVector(bounds.maxX, bounds.maxY, bounds.maxZ), getVector(bounds.maxX, bounds.maxY, bounds.minZ)), cols, still, Direction.UP, bx1, bx2, bz1, bz2));
			ret.add(createQuad(List.of(getVector(bounds.maxX, bounds.maxY, bounds.minZ), getVector(bounds.maxX, bounds.minY, bounds.minZ), getVector(bounds.minX, bounds.minY, bounds.minZ), getVector(bounds.minX, bounds.maxY, bounds.minZ)), cols, still, Direction.NORTH, bx1, bx2, by1, by2));
			ret.add(createQuad(List.of(getVector(bounds.minX, bounds.maxY, bounds.maxZ), getVector(bounds.minX, bounds.minY, bounds.maxZ), getVector(bounds.maxX, bounds.minY, bounds.maxZ), getVector(bounds.maxX, bounds.maxY, bounds.maxZ)), cols, still, Direction.SOUTH, bx1, bx2, by1, by2));
			ret.add(createQuad(List.of(getVector(bounds.minX, bounds.maxY, bounds.minZ), getVector(bounds.minX, bounds.minY, bounds.minZ), getVector(bounds.minX, bounds.minY, bounds.maxZ), getVector(bounds.minX, bounds.maxY, bounds.maxZ)), cols, still, Direction.WEST, bz1, bz2, by1, by2));
			ret.add(createQuad(List.of(getVector(bounds.maxX, bounds.maxY, bounds.maxZ), getVector(bounds.maxX, bounds.minY, bounds.maxZ), getVector(bounds.maxX, bounds.minY, bounds.minZ), getVector(bounds.maxX, bounds.maxY, bounds.minZ)), cols, still, Direction.EAST, bz1, bz2, by1, by2));
		}

		private Vector3f getVector(double x, double y, double z) {
			Vector3f ret = new Vector3f((float) x, (float) y, (float) z);
			rotate(ret, modelTransform.getRotation().getMatrix());
			return ret;
		}

		@Override
		public boolean useAmbientOcclusion() {
			return true;
		}

		@Override
		public boolean isGui3d() {
			return true;
		}

		@Override
		public boolean usesBlockLight() {
			return true;
		}

		@Override
		public boolean isCustomRenderer() {
			return true;
		}

		@SuppressWarnings("java:S1874") //don't have model data to pass in here and just calling getParticleTexture of baked model that doesn't need model data
		@Override
		public TextureAtlasSprite getParticleIcon() {
			//noinspection deprecation
			return models.get(ModelPart.BASE).getParticleIcon();
		}

		@Override
		public ItemOverrides getOverrides() {
			return overrideList;
		}

		@Override
		public BakedModel applyTransform(ItemTransforms.TransformType transformType, PoseStack poseStack, boolean applyLeftHandTransform) {
			if (transformType == ItemTransforms.TransformType.NONE) {
				return this;
			}

			ITEM_TRANSFORMS.getTransform(transformType).apply(applyLeftHandTransform, poseStack);

			return this;
		}

		@SuppressWarnings("deprecation")
		@Override
		public ItemTransforms getTransforms() {
			return ITEM_TRANSFORMS;
		}

		private BakedQuad createQuad(List<Vector3f> vecs, float[] colors, TextureAtlasSprite sprite, Direction face, float u1, float u2, float v1, float v2) {
			var bakedQuad = new BakedQuad[1];
			QuadBakingVertexConsumer quadBaker = new QuadBakingVertexConsumer(q -> bakedQuad[0] = q);
			quadBaker.setSprite(sprite);
			Vec3i dirVec = face.getNormal();
			quadBaker.setDirection(face);
			quadBaker.setTintIndex(-1);

			u1 = sprite.getU0() + u1 / 4f * sprite.uvShrinkRatio();
			u2 = sprite.getU0() + u2 / 4f * sprite.uvShrinkRatio();

			v1 = sprite.getV0() + v1 / 4f * sprite.uvShrinkRatio();
			v2 = sprite.getV0() + v2 / 4f * sprite.uvShrinkRatio();

			quadBaker.vertex(vecs.get(0).x(), vecs.get(0).y(), vecs.get(0).z()).color(colors[1], colors[2], colors[3], colors[0]).uv(u1, v1).normal(dirVec.getX(), dirVec.getY(), dirVec.getZ()).endVertex();
			quadBaker.vertex(vecs.get(1).x(), vecs.get(1).y(), vecs.get(1).z()).color(colors[1], colors[2], colors[3], colors[0]).uv(u1, v2).normal(dirVec.getX(), dirVec.getY(), dirVec.getZ()).endVertex();
			quadBaker.vertex(vecs.get(2).x(), vecs.get(2).y(), vecs.get(2).z()).color(colors[1], colors[2], colors[3], colors[0]).uv(u2, v2).normal(dirVec.getX(), dirVec.getY(), dirVec.getZ()).endVertex();
			quadBaker.vertex(vecs.get(3).x(), vecs.get(3).y(), vecs.get(3).z()).color(colors[1], colors[2], colors[3], colors[0]).uv(u2, v1).normal(dirVec.getX(), dirVec.getY(), dirVec.getZ()).endVertex();
			return bakedQuad[0];
		}

		private void rotate(Vector3f posIn, Matrix4f transformIn) {
			Vector3f originIn = new Vector3f(0.5f, 0.5f, 0.5f);
			Vector4f vector4f = new Vector4f(posIn.x() - originIn.x(), posIn.y() - originIn.y(), posIn.z() - originIn.z(), 1.0F);
			vector4f.transform(transformIn);
			posIn.set(vector4f.x() + originIn.x(), vector4f.y() + originIn.y(), vector4f.z() + originIn.z());
		}
	}

	private static class BackpackItemOverrideList extends ItemOverrides {
		private final BackpackDynamicModel.BackpackBakedModel backpackModel;

		public BackpackItemOverrideList(BackpackDynamicModel.BackpackBakedModel backpackModel) {
			this.backpackModel = backpackModel;
		}

		@Nullable
		@Override
		public BakedModel resolve(BakedModel model, ItemStack stack, @Nullable ClientLevel world, @Nullable LivingEntity livingEntity, int seed) {
			backpackModel.tankRight = false;
			backpackModel.tankLeft = false;
			backpackModel.battery = false;
			stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> {
				RenderInfo renderInfo = backpackWrapper.getRenderInfo();
				Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> tankRenderInfos = renderInfo.getTankRenderInfos();
				tankRenderInfos.forEach((pos, info) -> {
					if (pos == TankPosition.LEFT) {
						backpackModel.tankLeft = true;
						backpackModel.leftTankRenderInfo = info;
					} else {
						backpackModel.tankRight = true;
						backpackModel.rightTankRenderInfo = info;
					}
				});
				renderInfo.getBatteryRenderInfo().ifPresent(batteryRenderInfo -> {
					backpackModel.battery = true;
					backpackModel.batteryRenderInfo = batteryRenderInfo;
				});
			});

			return backpackModel;
		}
	}

	public static final class Loader implements IGeometryLoader<BackpackDynamicModel> {
		public static final Loader INSTANCE = new Loader();

		@Override
		public BackpackDynamicModel read(JsonObject modelContents, JsonDeserializationContext deserializationContext) {
			ImmutableMap.Builder<ModelPart, UnbakedModel> builder = ImmutableMap.builder();

			ImmutableMap.Builder<String, Either<Material, String>> texturesBuilder = ImmutableMap.builder();
			if (modelContents.has("clipsTexture")) {
				ResourceLocation clipsTexture = ResourceLocation.tryParse(modelContents.get("clipsTexture").getAsString());
				if (clipsTexture != null) {
					texturesBuilder.put("clips", Either.left(new Material(InventoryMenu.BLOCK_ATLAS, clipsTexture)));
				}
			}
			ImmutableMap<String, Either<Material, String>> textures = texturesBuilder.build();
			for (ModelPart part : ModelPart.values()) {
				addPartModel(builder, part, textures);
			}
			return new BackpackDynamicModel(builder.build());
		}

		private void addPartModel(ImmutableMap.Builder<ModelPart, UnbakedModel> builder, ModelPart modelPart, ImmutableMap<String, Either<Material, String>> textures) {
			builder.put(modelPart, new BlockModel(SophisticatedBackpacks.getRL("block/backpack_" + modelPart.name().toLowerCase(Locale.ENGLISH)), Collections.emptyList(), textures, true, null, ItemTransforms.NO_TRANSFORMS, Collections.emptyList()));
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
