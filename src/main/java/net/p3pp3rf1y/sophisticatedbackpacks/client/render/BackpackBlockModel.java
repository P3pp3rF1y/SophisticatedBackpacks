package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.geom.builders.UVPair;
import net.minecraft.client.renderer.block.BlockAndTintGetter;
import net.minecraft.client.renderer.block.FluidModel;
import net.minecraft.client.renderer.block.dispatch.BlockStateModelPart;
import net.minecraft.client.renderer.block.dispatch.ModelState;
import net.minecraft.client.renderer.block.dispatch.Variant;
import net.minecraft.client.renderer.chunk.ChunkSectionLayer;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.ModelBaker;
import net.minecraft.client.resources.model.ModelDebugName;
import net.minecraft.client.resources.model.ResolvedModel;
import net.minecraft.client.resources.model.SimpleModelWrapper;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.client.resources.model.cuboid.ItemTransforms;
import net.minecraft.client.resources.model.geometry.BakedQuad;
import net.minecraft.client.resources.model.geometry.QuadCollection;
import net.minecraft.client.resources.model.sprite.Material;
import net.minecraft.client.resources.model.sprite.TextureSlots;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.resources.Identifier;
import net.minecraft.util.GsonHelper;
import net.minecraft.util.Mth;
import net.minecraft.util.RandomSource;
import net.minecraft.util.context.ContextMap;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.phys.AABB;
import net.neoforged.neoforge.client.fluid.FluidTintSource;
import net.neoforged.neoforge.client.model.DynamicBlockStateModel;
import net.neoforged.neoforge.client.model.UnbakedModelLoader;
import net.neoforged.neoforge.client.model.block.CustomUnbakedBlockStateModel;
import net.neoforged.neoforge.client.model.pipeline.QuadBakingVertexConsumer;
import net.neoforged.neoforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import org.jspecify.annotations.Nullable;

import java.util.*;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;

public class BackpackBlockModel implements UnbakedModel {
	public static final ItemDisplayContext WORN = ItemDisplayContext.valueOf("SOPHISTICATEDBACKPACKS_WORN");
	@Nullable
	private final Identifier parent;
	private final Map<ModelPart, UnbakedModel> modelParts;
	@Nullable
	private final ItemTransforms itemTransforms;

	private BackpackBlockModel(@Nullable Identifier parent, Map<ModelPart, UnbakedModel> modelParts, @Nullable ItemTransforms itemtransforms) {
		this.parent = parent;
		this.modelParts = modelParts;
		this.itemTransforms = itemtransforms;
	}

	public BlockStateModel bakeBlockStateModel(ModelBaker baker, ResolvedModel resolvedModel, ModelState modelState) {
		ImmutableMap.Builder<ModelPart, QuadCollection> builder = ImmutableMap.builder();
		modelParts.forEach((part, model) -> {
			// noinspection DataFlowIssue - the model is constructed in the Loader class below and will always have parent
			builder.put(part, baker.getModel(model.parent()).getTopGeometry().bake(getTextureSlots(baker, model, resolvedModel), baker, modelState,
					resolvedModel, ContextMap.EMPTY));
		});
		return new BlockStateModel(builder.build(), modelState, this.itemTransforms,
				resolvedModel.resolveParticleMaterial(getTextureSlots(baker, modelParts.get(ModelPart.BASE), resolvedModel), baker));
	}

	private TextureSlots getTextureSlots(ModelBaker baker, UnbakedModel partModel, ModelDebugName debugName) {
		TextureSlots.Resolver resolver = new TextureSlots.Resolver();

		resolver.addLast(partModel.textureSlots());

		Identifier parent = partModel.parent();
		if (parent != null) {
			ResolvedModel resolvedParent = baker.getModel(parent);
			while (resolvedParent != null) {
				resolver.addLast(resolvedParent.wrapped().textureSlots());
				resolvedParent = resolvedParent.parent();
			}
		}
		return resolver.resolve(debugName);
	}

	@Override
	public @Nullable ItemTransforms transforms() {
		return itemTransforms;
	}

	@Override
	public @Nullable Identifier parent() {
		return parent;
	}

	@Override
	public void resolveDependencies(Resolver resolver) {
		modelParts.values().forEach(model -> {
			Identifier parent = model.parent();
			if (parent != null) {
				resolver.markDependency(parent);
			}
			model.resolveDependencies(resolver);
		});
	}

	public static final class BlockStateModel implements DynamicBlockStateModel {
		private int cachedLeftTankSteps = -1;
		private final Map<FluidCacheKey, QuadCollection> leftTankFluidCache = new HashMap<>();
		private int cachedRightTankSteps = -1;
		private final Map<FluidCacheKey, QuadCollection> rightTankFluidCache = new HashMap<>();
		private int cachedBatterySteps = -1;
		private final Map<Integer, QuadCollection> batteryChargeCache = new HashMap<>();
		private ItemDisplayContext lastContext = ItemDisplayContext.NONE;

		private final Map<ModelPart, QuadCollection> models;
		private final ModelState modelState;
		private final Material.Baked particleMaterial;
		@Nullable
		private AABB leftTankFluidBounds;
		@Nullable
		private AABB rightTankFluidBounds;
		@Nullable
		private AABB batteryChargeBounds;

		public boolean tankLeft;
		public RenderData.@Nullable TankRenderData leftTankRenderData = null;
		public boolean tankRight;
		public RenderData.@Nullable TankRenderData rightTankRenderData = null;
		public boolean battery;
		public RenderData.@Nullable BatteryRenderData batteryRenderData = null;
		private final ItemTransforms itemTransforms;

		public BlockStateModel(Map<ModelPart, QuadCollection> models, ModelState modelState, ItemTransforms itemTransforms, Material.Baked particleMaterial) {
			this.models = models;
			this.modelState = modelState;
			this.particleMaterial = particleMaterial;
			this.itemTransforms = itemTransforms;
		}

		@Nullable
		public BakedQuad getDisplayItemQuad() {
			QuadCollection displayItemModel = models.get(ModelPart.DISPLAY_ITEM);
			if (displayItemModel == null) {
				return null;
			}
			List<BakedQuad> quads = displayItemModel.getQuads(null);
			if (quads.isEmpty()) {
				return null;
			}
			return quads.getFirst();
		}

		@Override
		public void collectParts(@Nullable BlockAndTintGetter level, BlockPos pos, @Nullable BlockState state, RandomSource rand,
				List<BlockStateModelPart> parts) {
			tankLeft = false;
			leftTankRenderData = null;
			tankRight = false;
			rightTankRenderData = null;
			battery = false;
			batteryRenderData = null;

			if (state != null && state.hasProperty(LEFT_TANK) && state.hasProperty(RIGHT_TANK) && state.hasProperty(BATTERY)) {
				tankLeft = state.getValue(LEFT_TANK);
				tankRight = state.getValue(RIGHT_TANK);
				battery = state.getValue(BATTERY);

				if ((tankLeft || tankRight || battery) && level != null) {
					level.getBlockEntity(pos, ModBlocks.BACKPACK_TILE_TYPE.get()).ifPresent(backpackBlockEntity -> {
						RenderDataHandler renderDataHandler = backpackBlockEntity.getBackpackWrapper().getRenderDataHandler();
						Map<TankPosition, RenderData.TankRenderData> tankRenderData = renderDataHandler.getTankRenderData();
						tankRenderData.forEach((tankPos, renderData) -> {
							if (tankPos == TankPosition.LEFT) {
								leftTankRenderData = renderData;
							} else {
								rightTankRenderData = renderData;
							}
						});
						renderDataHandler.getBatteryRenderData().ifPresent(renderData -> {
							batteryRenderData = renderData;
						});
					});
				}
			}

			collectPartsNoStateUpdate(parts);
		}

		private void collectPartsNoStateUpdate(List<BlockStateModelPart> parts) {
			collectPartsNoStateUpdate(parts, lastContext);
		}

		private void collectPartsNoStateUpdate(List<BlockStateModelPart> parts, ItemDisplayContext context) {
			QuadCollection.Builder builder = new QuadCollection.Builder();
			QuadCollection.Builder translucentBuilder = new QuadCollection.Builder();
			builder.addAll(models.get(ModelPart.BASE));
			addLeftSide(builder, translucentBuilder);
			addRightSide(builder, translucentBuilder);
			addFront(builder);

			if (context != WORN) {
				builder.addAll(models.get(ModelPart.STRAPS));
			}

			parts.add(new SimpleModelWrapper(builder.build(), true, particleMaterial));
			parts.add(new SimpleModelWrapper(translucentBuilder.build(), true, new Material.Baked(particleMaterial.sprite(), true)));
		}

		private void addFront(QuadCollection.Builder builder) {
			if (battery) {
				if (batteryRenderData != null && batteryRenderData.chargeRatio() != 0) {
					float ratio = batteryRenderData.chargeRatio();

					if (cachedBatterySteps < 0) {
						builder.addAll(getBatteryChargeQuads(ratio));
					} else {
						int step = ratioToStep(ratio, cachedBatterySteps);
						builder.addAll(batteryChargeCache.computeIfAbsent(step, s -> getBatteryChargeQuads(stepToRatio(s, cachedBatterySteps))));
					}
				}
				builder.addAll(models.get(ModelPart.BATTERY));
			} else {
				builder.addAll(models.get(ModelPart.FRONT_POUCH));
			}
		}

		private QuadCollection getBatteryChargeQuads(float chargeRatio) {
			QuadCollection chargeModel = models.get(ModelPart.BATTERY_CHARGE);
			if (chargeModel == null) {
				return QuadCollection.EMPTY;
			}

			List<BakedQuad> src = chargeModel.getAll();
			if (src.isEmpty()) {
				return QuadCollection.EMPTY;
			}

			Direction.Axis batteryFillAxis = getHorizontalFillAxis(src);
			if (cachedBatterySteps < 0) {
				cachedBatterySteps = computeStepsFromModelUV(src, batteryFillAxis);
				if (cachedBatterySteps <= 0) {
					batteryFillAxis = batteryFillAxis == Direction.Axis.X ? Direction.Axis.Z : Direction.Axis.X;
					cachedBatterySteps = computeStepsFromModelUV(src, batteryFillAxis);
				}
			}

			AABB bounds = batteryChargeBounds != null ? batteryChargeBounds : computeBoundsFromQuads(src);
			batteryChargeBounds = bounds;
			if (bounds == null) {
				return QuadCollection.EMPTY;
			}

			int step = ratioToStep(chargeRatio, cachedBatterySteps);
			if (step <= 0) {
				return QuadCollection.EMPTY;
			}
			if (step >= cachedBatterySteps) {
				step = cachedBatterySteps;
			}

			// Avoid exact 1.0 slice for full charge to prevent edge-case clipping/z-fighting artifacts on custom pack quads.
			float stepRatio = step >= cachedBatterySteps ? 0.9999f : stepToRatio(step, cachedBatterySteps);

			SliceSpec s = horizontalSliceSpecFromUv(src, bounds, stepRatio, batteryFillAxis);
			return sliceQuadsAxis(src, s.axis(), s.cut(), s.keepGreaterOrEqual());
		}

		private record SliceSpec(Direction.Axis axis, double cut, boolean keepGreaterOrEqual) {
		}

		private static SliceSpec horizontalSliceSpecFromUv(List<BakedQuad> quads, AABB b, float ratio, Direction.Axis axis) {
			ratio = Mth.clamp(ratio, 0f, 1f);
			boolean lowUAtLowCoord = isLowUAtLowCoord(quads, axis);
			return switch (axis) {
				case X -> lowUAtLowCoord
						? new SliceSpec(Direction.Axis.X, b.minX + (b.maxX - b.minX) * ratio, false)
						: new SliceSpec(Direction.Axis.X, b.maxX - (b.maxX - b.minX) * ratio, true);
				case Z -> lowUAtLowCoord
						? new SliceSpec(Direction.Axis.Z, b.minZ + (b.maxZ - b.minZ) * ratio, false)
						: new SliceSpec(Direction.Axis.Z, b.maxZ - (b.maxZ - b.minZ) * ratio, true);
				default -> new SliceSpec(Direction.Axis.X, b.minX + (b.maxX - b.minX) * ratio, false);
			};
		}

		private static Direction.Axis getHorizontalFillAxis(List<BakedQuad> quads) {
			int xSteps = computeStepsFromModelUV(quads, Direction.Axis.X);
			int zSteps = computeStepsFromModelUV(quads, Direction.Axis.Z);
			return zSteps > xSteps ? Direction.Axis.Z : Direction.Axis.X;
		}

		private static boolean isLowUAtLowCoord(List<BakedQuad> quads, Direction.Axis axis) {
			float minU = Float.POSITIVE_INFINITY;
			float maxU = Float.NEGATIVE_INFINITY;
			float coordAtMinU = 0f;
			float coordAtMaxU = 0f;
			boolean found = false;

			for (BakedQuad q : quads) {
				for (int i = 0; i < 4; i++) {
					float x = q.position(i).x();
					float z = q.position(i).z();
					float u = UVPair.unpackU(q.packedUV(i));
					float coord = axis == Direction.Axis.Z ? z : x;
					if (u < minU) {
						minU = u;
						coordAtMinU = coord;
						found = true;
					}
					if (u > maxU) {
						maxU = u;
						coordAtMaxU = coord;
						found = true;
					}
				}
			}

			return !found || coordAtMinU <= coordAtMaxU;
		}

		private void addRightSide(QuadCollection.Builder builder, QuadCollection.Builder translucentBuilder) {
			if (tankRight) {
				RenderData.TankRenderData rightTankRenderData = this.rightTankRenderData;
				if (rightTankRenderData != null && rightTankRenderData.fillRatio() != 0) {
					rightTankRenderData.getFluid().ifPresent(fluid -> {
						if (cachedRightTankSteps != -1) {
							FluidCacheKey cacheKey = getFluidCacheKey(fluid, cachedRightTankSteps, rightTankRenderData.fillRatio());
							if (rightTankFluidCache.containsKey(cacheKey)) {
								translucentBuilder.addAll(rightTankFluidCache.get(cacheKey));
								return;
							}
						}
						QuadCollection fluidQuads = getTankFluidFromModel(ModelPart.RIGHT_TANK_FLUID, fluid, rightTankRenderData.fillRatio(), false);
						rightTankFluidCache.put(getFluidCacheKey(fluid, cachedRightTankSteps, rightTankRenderData.fillRatio()), fluidQuads);
						translucentBuilder.addAll(fluidQuads);
					});
				}
				builder.addAll(models.get(ModelPart.RIGHT_TANK));
			} else {
				builder.addAll(models.get(ModelPart.RIGHT_POUCH));
			}
		}

		private void addLeftSide(QuadCollection.Builder builder, QuadCollection.Builder translucentBuilder) {
			if (tankLeft) {
				RenderData.TankRenderData leftTankRenderData = this.leftTankRenderData;
				if (leftTankRenderData != null && leftTankRenderData.fillRatio() != 0) {
					leftTankRenderData.getFluid().ifPresent(fluid -> {
						if (cachedLeftTankSteps != -1) {
							FluidCacheKey cacheKey = getFluidCacheKey(fluid, cachedLeftTankSteps, leftTankRenderData.fillRatio());
							if (leftTankFluidCache.containsKey(cacheKey)) {
								translucentBuilder.addAll(leftTankFluidCache.get(cacheKey));
								return;
							}
						}
						QuadCollection fluidQuads = getTankFluidFromModel(ModelPart.LEFT_TANK_FLUID, fluid, leftTankRenderData.fillRatio(), true);
						leftTankFluidCache.put(getFluidCacheKey(fluid, cachedLeftTankSteps, leftTankRenderData.fillRatio()), fluidQuads);
						translucentBuilder.addAll(fluidQuads);
					});
				}
				builder.addAll(models.get(ModelPart.LEFT_TANK));
			} else {
				builder.addAll(models.get(ModelPart.LEFT_POUCH));
			}
		}

		private FluidCacheKey getFluidCacheKey(FluidStack fluid, int cachedSteps, float fillRatio) {
			int step = cachedSteps > 0 ? ratioToStep(fillRatio, cachedSteps) : 0;
			return new FluidCacheKey(fluid.getFluid(), fluid.getComponents(), step);
		}

		private record FluidCacheKey(Fluid fluid, DataComponentMap components, int step) {
		}

		private QuadCollection getTankFluidFromModel(ModelPart fluidPart, FluidStack fluidStack, float ratio, boolean isLeft) {
			if (fluidStack == FluidStack.EMPTY || Mth.equal(ratio, 0f)) {
				return QuadCollection.EMPTY;
			}

			QuadCollection fluidModel = models.get(fluidPart);
			if (fluidModel == null) {
				return QuadCollection.EMPTY;
			}

			List<BakedQuad> src = fluidModel.getAll();
			if (src.isEmpty()) {
				return QuadCollection.EMPTY;
			}

			int steps = isLeft ? cachedLeftTankSteps : cachedRightTankSteps;
			if (steps < 0) {
				steps = computeStepsFromModelUV(src, Direction.Axis.Y);
				if (isLeft)
					cachedLeftTankSteps = steps;
				else
					cachedRightTankSteps = steps;
			}

			AABB cached = isLeft ? leftTankFluidBounds : rightTankFluidBounds;
			AABB max = cached != null ? cached : computeBoundsFromQuads(src);
			if (isLeft) {
				leftTankFluidBounds = max;
			} else {
				rightTankFluidBounds = max;
			}
			if (max == null) {
				return QuadCollection.EMPTY;
			}

			int step = ratioToStep(ratio, steps);
			if (step <= 0) {
				return QuadCollection.EMPTY;
			}
			if (step >= steps) {
				step = steps;
			}

			float stepRatio = stepToRatio(step, steps);

			double cut = max.minY + (max.maxY - max.minY) * stepRatio;

			List<BakedQuad> sliced = sliceQuadsAxis(src, Direction.Axis.Y, cut, false).getAll();
			if (sliced.isEmpty()) {
				return QuadCollection.EMPTY;
			}

			FluidState fluidState = fluidStack.getFluid().defaultFluidState();
			FluidModel fluidStateModel = Minecraft.getInstance().getModelManager().getFluidStateModelSet().get(fluidState);
			TextureAtlasSprite newSprite = fluidStateModel.stillMaterial().sprite();
			int argb = fluidStateModel.tintSource() instanceof FluidTintSource fluidTintSource
					? fluidTintSource.colorAsStack(fluidStack)
					: fluidStateModel.tintSource() != null ? fluidStateModel.tintSource().color(fluidState.createLegacyBlock()) : -1;

			QuadCollection.Builder builder = respriteAndTintQuads(sliced, newSprite, argb);

			BakedQuad top = buildFluidTopQuad((float) cut, max, src, newSprite, argb);
			if (top != null) {
				builder.addUnculledFace(top);
			}

			return builder.build();
		}

		@Nullable
		private static BakedQuad buildFluidTopQuad(float y, AABB max, List<BakedQuad> src, TextureAtlasSprite sprite, int argb) {
			if (y <= (float) max.minY + 1e-6f) {
				return null;
			}

			float a = (argb >>> 24 & 0xFF) / 255f;
			float r = (argb >>> 16 & 0xFF) / 255f;
			float g = (argb >>> 8 & 0xFF) / 255f;
			float b = (argb & 0xFF) / 255f;

			double pxPerUnitX = computePixelsPerUnitFromU(src, Direction.NORTH, Direction.SOUTH, Direction.Axis.X);
			double pxPerUnitZ = computePixelsPerUnitFromU(src, Direction.EAST, Direction.WEST, Direction.Axis.Z);

			if (pxPerUnitX <= 0) {
				pxPerUnitX = 16.0;
			}
			if (pxPerUnitZ <= 0) {
				pxPerUnitZ = pxPerUnitX;
			}

			double widthX = (max.maxX - max.minX);
			double depthZ = (max.maxZ - max.minZ);

			double uPixels = widthX * pxPerUnitX;
			double vPixels = depthZ * pxPerUnitZ;

			int sw = sprite.contents().width();
			int sh = sprite.contents().height();
			if (sw <= 0 || sh <= 0) {
				return null;
			}

			float u0 = sprite.getU0();
			float v0 = sprite.getV0();
			float du = (float) ((uPixels / sw) * (sprite.getU1() - sprite.getU0()));
			float dv = (float) ((vPixels / sh) * (sprite.getV1() - sprite.getV0()));
			float u1 = u0 + du;
			float v1 = v0 + dv;

			QuadBakingVertexConsumer qb = new QuadBakingVertexConsumer();
			qb.setSprite(sprite, ChunkSectionLayer.TRANSLUCENT, net.minecraft.client.renderer.Sheets.translucentBlockItemSheet());
			qb.setDirection(Direction.UP);
			qb.setTintIndex(-1);
			qb.setShade(false);
			qb.setAmbientOcclusion(false);
			Vec3i n = Direction.UP.getUnitVec3i();

			float x0 = (float) max.minX;
			float x1 = (float) max.maxX;
			float z0 = (float) max.minZ;
			float z1 = (float) max.maxZ;

			qb.addVertex(x0, y, z0).setColor(r, g, b, a).setUv(u0, v0).setNormal(n.getX(), n.getY(), n.getZ());
			qb.addVertex(x0, y, z1).setColor(r, g, b, a).setUv(u0, v1).setNormal(n.getX(), n.getY(), n.getZ());
			qb.addVertex(x1, y, z1).setColor(r, g, b, a).setUv(u1, v1).setNormal(n.getX(), n.getY(), n.getZ());
			qb.addVertex(x1, y, z0).setColor(r, g, b, a).setUv(u1, v0).setNormal(n.getX(), n.getY(), n.getZ());

			return qb.bakeQuad();
		}

		private static double computePixelsPerUnitFromU(List<BakedQuad> quads, Direction d1, Direction d2, Direction.Axis modelAxis) {
			double bestSpan = -1;
			double bestPxPerUnit = -1;

			for (BakedQuad q : quads) {
				Direction dir = q.direction();
				if (dir != d1 && dir != d2) {
					continue;
				}

				float minCoord = Float.POSITIVE_INFINITY;
				float maxCoord = Float.NEGATIVE_INFINITY;
				float minU = Float.POSITIVE_INFINITY;
				float maxU = Float.NEGATIVE_INFINITY;

				for (int i = 0; i < 4; i++) {
					float x = q.position(i).x();
					float y = q.position(i).y();
					float z = q.position(i).z();
					float u = UVPair.unpackU(q.packedUV(i));

					float coord = switch (modelAxis) {
						case X -> x;
						case Y -> y;
						case Z -> z;
					};

					minCoord = Math.min(minCoord, coord);
					maxCoord = Math.max(maxCoord, coord);
					minU = Math.min(minU, u);
					maxU = Math.max(maxU, u);
				}

				double modelSpan = maxCoord - minCoord;
				if (modelSpan <= 1e-6) {
					continue;
				}

				TextureAtlasSprite s = q.materialInfo().sprite();
				if (s == null) {
					continue;
				}

				double uDen = (s.getU1() - s.getU0());
				if (Math.abs(uDen) < 1e-9) {
					continue;
				}

				double uNormSpan = Math.abs(maxU - minU) / uDen;
				int texW = s.contents().width();
				if (texW <= 0) {
					continue;
				}

				double uPixels = uNormSpan * texW;
				double pxPerUnit = uPixels / modelSpan;

				if (modelSpan > bestSpan) {
					bestSpan = modelSpan;
					bestPxPerUnit = pxPerUnit;
				}
			}

			return bestPxPerUnit;
		}

		private static int ratioToStep(float ratio, int steps) {
			ratio = Mth.clamp(ratio, 0f, 1f);
			if (steps <= 0) {
				return 0;
			}
			int step = Mth.floor(ratio * steps + 1e-6f);
			return Mth.clamp(step, 0, steps);
		}

		private static float stepToRatio(int step, int steps) {
			return steps <= 0 ? 0f : (step / (float) steps);
		}

		@Nullable
		private static AABB computeBoundsFromQuads(List<BakedQuad> quads) {
			if (quads.isEmpty()) {
				return null;
			}
			float minX = Float.POSITIVE_INFINITY, minY = Float.POSITIVE_INFINITY, minZ = Float.POSITIVE_INFINITY;
			float maxX = Float.NEGATIVE_INFINITY, maxY = Float.NEGATIVE_INFINITY, maxZ = Float.NEGATIVE_INFINITY;

			for (BakedQuad q : quads) {
				for (int i = 0; i < 4; i++) {
					float x = q.position(i).x();
					float y = q.position(i).y();
					float z = q.position(i).z();
					minX = Math.min(minX, x);
					minY = Math.min(minY, y);
					minZ = Math.min(minZ, z);
					maxX = Math.max(maxX, x);
					maxY = Math.max(maxY, y);
					maxZ = Math.max(maxZ, z);
				}
			}
			if (!Float.isFinite(minX) || !Float.isFinite(minY) || !Float.isFinite(minZ)) {
				return null;
			}
			return new AABB(minX, minY, minZ, maxX, maxY, maxZ);
		}

		private static QuadCollection sliceQuadsAxis(List<BakedQuad> src, Direction.Axis axis, double cut, boolean keepGreaterOrEqual) {
			QuadCollection.Builder builder = new QuadCollection.Builder();
			for (BakedQuad q : src) {
				BakedQuad sliced = sliceQuadAxis(q, axis, (float) cut, keepGreaterOrEqual);
				if (sliced != null) {
					builder.addUnculledFace(sliced);
				}
			}
			return builder.build();
		}

		@Nullable
		private static BakedQuad sliceQuadAxis(BakedQuad q, Direction.Axis axis, float cut, boolean keepGreaterOrEqual) {
			Vert[] in = new Vert[4];
			for (int i = 0; i < 4; i++) {
				in[i] = new Vert(q.position(i).x(), q.position(i).y(), q.position(i).z(), UVPair.unpackU(q.packedUV(i)), UVPair.unpackV(q.packedUV(i)));
			}

			List<Vert> out = clipAgainstPlane(Arrays.asList(in), axis, cut, keepGreaterOrEqual);

			if (out.isEmpty())
				return null;

			while (out.size() < 4) {
				out.add(out.get(out.size() - 1));
			}
			if (out.size() > 4) {
				out = out.subList(0, 4);
			}

			QuadBakingVertexConsumer qb = new QuadBakingVertexConsumer();
			BakedQuad.MaterialInfo materialInfo = q.materialInfo();
			qb.setSprite(materialInfo.sprite(), materialInfo.layer(), materialInfo.itemRenderType());
			qb.setDirection(q.direction());
			qb.setTintIndex(materialInfo.tintIndex());
			qb.setShade(materialInfo.shade());
			qb.setLightEmission(materialInfo.lightEmission());
			qb.setAmbientOcclusion(materialInfo.ambientOcclusion());
			Vec3i n = q.direction().getUnitVec3i();

			for (int i = 0; i < 4; i++) {
				Vert p = out.get(i);
				qb.addVertex(p.x, p.y, p.z).setColor(1f, 1f, 1f, 1f).setUv(p.u, p.v).setNormal(n.getX(), n.getY(), n.getZ());
			}

			return qb.bakeQuad();
		}

		private static List<Vert> clipAgainstPlane(List<Vert> poly, Direction.Axis axis, float cut, boolean keepGE) {
			List<Vert> out = new ArrayList<>(poly.size() + 2);

			Vert prev = poly.get(poly.size() - 1);
			boolean prevIn = inside(prev, axis, cut, keepGE);

			for (Vert cur : poly) {
				boolean curIn = inside(cur, axis, cut, keepGE);

				if (prevIn && curIn) {
					out.add(cur);
				} else if (prevIn && !curIn) {
					out.add(intersect(prev, cur, axis, cut));
				} else if (!prevIn && curIn) {
					out.add(intersect(prev, cur, axis, cut));
					out.add(cur);
				}

				prev = cur;
				prevIn = curIn;
			}

			return out;
		}

		private static boolean inside(Vert p, Direction.Axis axis, float cut, boolean keepGE) {
			float c = switch (axis) {
				case X -> p.x;
				case Y -> p.y;
				case Z -> p.z;
			};
			float eps = 1e-6f;
			return keepGE ? (c + eps >= cut) : (c <= cut + eps);
		}

		private static Vert intersect(Vert a, Vert b, Direction.Axis axis, float cut) {
			float ca = switch (axis) {
				case X -> a.x;
				case Y -> a.y;
				case Z -> a.z;
			};
			float cb = switch (axis) {
				case X -> b.x;
				case Y -> b.y;
				case Z -> b.z;
			};

			float denom = (cb - ca);
			float t = denom == 0f ? 0f : (cut - ca) / denom;
			t = Mth.clamp(t, 0f, 1f);

			return new Vert(Mth.lerp(t, a.x, b.x), Mth.lerp(t, a.y, b.y), Mth.lerp(t, a.z, b.z), Mth.lerp(t, a.u, b.u), Mth.lerp(t, a.v, b.v));
		}

		private record Vert(float x, float y, float z, float u, float v) {
		}

		private static QuadCollection.Builder respriteAndTintQuads(List<BakedQuad> src, TextureAtlasSprite newSprite, int argb) {
			float a = (argb >>> 24 & 0xFF) / 255f;
			float r = (argb >>> 16 & 0xFF) / 255f;
			float g = (argb >>> 8 & 0xFF) / 255f;
			float b = (argb & 0xFF) / 255f;
			float[] cols = {a, r, g, b};
			QuadCollection.Builder builder = new QuadCollection.Builder();
			for (BakedQuad q : src) {
				builder.addUnculledFace(respriteAndTintQuad(q, newSprite, cols));
			}
			return builder;
		}

		private static BakedQuad respriteAndTintQuad(BakedQuad q, TextureAtlasSprite newSprite, float[] cols) {
			TextureAtlasSprite oldSprite = q.materialInfo().sprite();
			BakedQuad.MaterialInfo materialInfo = q.materialInfo();

			QuadBakingVertexConsumer qb = new QuadBakingVertexConsumer();
			qb.setSprite(newSprite, materialInfo.layer(), materialInfo.itemRenderType());
			qb.setDirection(q.direction());
			qb.setTintIndex(-1);
			qb.setShade(materialInfo.shade());
			qb.setLightEmission(materialInfo.lightEmission());
			qb.setAmbientOcclusion(materialInfo.ambientOcclusion());

			Vec3i n = q.direction().getUnitVec3i();

			for (int vi = 0; vi < 4; vi++) {
				float x = q.position(vi).x();
				float y = q.position(vi).y();
				float z = q.position(vi).z();

				float uOld = UVPair.unpackU(q.packedUV(vi));
				float vOld = UVPair.unpackV(q.packedUV(vi));

				float uNew = remapU(oldSprite, newSprite, uOld);
				float vNew = remapV(oldSprite, newSprite, vOld);

				int packedUv2 = materialInfo.lightEmission();
				int lightU = packedUv2 & 0xFFFF;
				int lightV = (packedUv2 >>> 16) & 0xFFFF;

				qb.addVertex(x, y, z).setColor(cols[1], cols[2], cols[3], cols[0]).setUv(uNew, vNew).setUv2(lightU, lightV).setNormal(n.getX(), n.getY(),
						n.getZ());
			}

			return qb.bakeQuad();
		}

		private static float remapU(TextureAtlasSprite oldS, TextureAtlasSprite newS, float u) {
			float denom = (oldS.getU1() - oldS.getU0());
			if (denom == 0f) {
				return newS.getU0();
			}
			float t = (u - oldS.getU0()) / denom;
			return newS.getU0() + t * (newS.getU1() - newS.getU0());
		}

		private static float remapV(TextureAtlasSprite oldS, TextureAtlasSprite newS, float v) {
			float denom = (oldS.getV1() - oldS.getV0());
			if (denom == 0f) {
				return newS.getV0();
			}
			float t = (v - oldS.getV0()) / denom;
			return newS.getV0() + t * (newS.getV1() - newS.getV0());
		}

		private static int computeStepsFromModelUV(List<BakedQuad> quads, Direction.Axis fillAxis) {
			if (quads.isEmpty())
				return 0;

			double bestModelSpan = -1;
			double bestPixelSpan = -1;

			for (BakedQuad q : quads) {
				if (fillAxis == Direction.Axis.Y && (q.direction() == Direction.UP || q.direction() == Direction.DOWN)) {
					continue;
				}

				float minCoord = Float.POSITIVE_INFINITY;
				float maxCoord = Float.NEGATIVE_INFINITY;

				float minU = Float.POSITIVE_INFINITY, maxU = Float.NEGATIVE_INFINITY;
				float minV = Float.POSITIVE_INFINITY, maxV = Float.NEGATIVE_INFINITY;

				for (int i = 0; i < 4; i++) {
					float x = q.position(i).x();
					float y = q.position(i).y();
					float z = q.position(i).z();

					long packedUV = q.packedUV(i);
					float u = UVPair.unpackU(packedUV);
					float vv = UVPair.unpackV(packedUV);

					float coord = switch (fillAxis) {
						case X -> x;
						case Y -> y;
						case Z -> z;
					};

					minCoord = Math.min(minCoord, coord);
					maxCoord = Math.max(maxCoord, coord);

					minU = Math.min(minU, u);
					maxU = Math.max(maxU, u);
					minV = Math.min(minV, vv);
					maxV = Math.max(maxV, vv);
				}

				double modelSpan = maxCoord - minCoord;
				if (modelSpan <= 1e-6)
					continue;

				TextureAtlasSprite s = q.materialInfo().sprite();

				double pixelSpan;
				if (fillAxis == Direction.Axis.X) {
					double denom = (s.getU1() - s.getU0());
					if (Math.abs(denom) < 1e-9)
						continue;
					double uNormSpan = Math.abs(maxU - minU) / denom;
					int texW = s.contents().width();
					pixelSpan = uNormSpan * texW;
				} else if (fillAxis == Direction.Axis.Y) {
					double denom = (s.getV1() - s.getV0());
					if (Math.abs(denom) < 1e-9)
						continue;
					double vNormSpan = Math.abs(maxV - minV) / denom;
					int texH = s.contents().height();
					pixelSpan = vNormSpan * texH;
				} else {
					double denom = (s.getU1() - s.getU0());
					if (Math.abs(denom) < 1e-9)
						continue;
					double uNormSpan = Math.abs(maxU - minU) / denom;
					int texW = s.contents().width();
					pixelSpan = uNormSpan * texW;
				}

				if (modelSpan > bestModelSpan) {
					bestModelSpan = modelSpan;
					bestPixelSpan = pixelSpan;
				}
			}

			if (bestPixelSpan <= 0)
				return 0;

			return Mth.clamp((int) Math.round(bestPixelSpan), 1, 64);
		}

		@Override
		public Material.Baked particleMaterial() {
			return particleMaterial;
		}

		@Override
		public int materialFlags() {
			return models.values().stream().mapToInt(QuadCollection::materialFlags).reduce(0, (a, b) -> a | b);
		}

		public List<BakedQuad> getQuads() {
			return getQuads(lastContext);
		}

		public List<BakedQuad> getQuads(ItemDisplayContext context) {
			List<BlockStateModelPart> parts = new ArrayList<>();
			collectPartsNoStateUpdate(parts, context);

			List<BakedQuad> bakedQuads = new ArrayList<>();

			for (BlockStateModelPart part : parts) {
				for (Direction dir : Direction.values()) {
					bakedQuads.addAll(part.getQuads(dir));
				}
				bakedQuads.addAll(part.getQuads(null));
			}

			return bakedQuads;
		}
	}

	public record UnbakedBlockStateModel(Variant variant) implements CustomUnbakedBlockStateModel {
		public static final MapCodec<UnbakedBlockStateModel> CODEC = RecordCodecBuilder.mapCodec(
				instance -> instance.group(Variant.MAP_CODEC.forGetter(UnbakedBlockStateModel::variant)).apply(instance, UnbakedBlockStateModel::new));
		public static final Identifier ID = SophisticatedBackpacks.getIdentifier("backpack_model_loader");

		@Override
		public BlockStateModel bake(ModelBaker modelBaker) {
			ResolvedModel resolvedModel = modelBaker.getModel(variant.modelLocation());
			if (resolvedModel.wrapped() instanceof BackpackBlockModel model) {
				return model.bakeBlockStateModel(modelBaker, resolvedModel, variant.modelState().asModelState());
			}

			throw new IllegalStateException("Expected BackpackBlockModel, but got " + resolvedModel.wrapped().getClass().getName());
		}

		@Override
		public void resolveDependencies(Resolver resolver) {
			resolver.markDependency(variant.modelLocation());
		}

		@Override
		public MapCodec<? extends CustomUnbakedBlockStateModel> codec() {
			return CODEC;
		}
	}

	public static final class Loader implements UnbakedModelLoader<BackpackBlockModel> {
		public static final Loader INSTANCE = new Loader();

		@Override
		public BackpackBlockModel read(JsonObject modelContents, JsonDeserializationContext deserializationContext) {
			ImmutableMap.Builder<ModelPart, UnbakedModel> builder = ImmutableMap.builder();

			ItemTransforms itemTransforms = null;
			if (modelContents.has("display")) {
				JsonObject displayJson = GsonHelper.getAsJsonObject(modelContents, "display");
				itemTransforms = deserializationContext.deserialize(displayJson, ItemTransforms.class);
			}

			TextureSlots.Data textures = getTextureMap(modelContents);
			for (ModelPart part : ModelPart.values()) {
				addPartModel(builder, part, textures);
			}
			Identifier parent = modelContents.has("parent") ? Identifier.parse(GsonHelper.getAsString(modelContents, "parent")) : null;
			return new BackpackBlockModel(parent, builder.build(), itemTransforms);
		}

		private TextureSlots.Data getTextureMap(JsonObject modelContents) {
			if (modelContents.has("textures")) {
				JsonObject texturesJson = GsonHelper.getAsJsonObject(modelContents, "textures");
				return TextureSlots.parseTextureMap(texturesJson);
			} else {
				return TextureSlots.Data.EMPTY;
			}
		}

		private void addPartModel(ImmutableMap.Builder<ModelPart, UnbakedModel> builder, ModelPart modelPart, TextureSlots.Data textures) {
			builder.put(modelPart,
					new PartModel(SophisticatedBackpacks.getIdentifier("block/backpack_" + modelPart.name().toLowerCase(Locale.ENGLISH)), textures));
		}
	}

	public enum ModelPart {
		BASE, BATTERY, FRONT_POUCH, LEFT_POUCH, LEFT_TANK, RIGHT_POUCH, RIGHT_TANK, STRAPS, LEFT_TANK_FLUID, RIGHT_TANK_FLUID, BATTERY_CHARGE, DISPLAY_ITEM
	}

	private record PartModel(Identifier parent, TextureSlots.Data textures) implements UnbakedModel {
		@Override
		public Boolean ambientOcclusion() {
			return true;
		}

		@Override
		public ItemTransforms transforms() {
			return ItemTransforms.NO_TRANSFORMS;
		}

		@Override
		public TextureSlots.Data textureSlots() {
			return textures;
		}
	}
}
