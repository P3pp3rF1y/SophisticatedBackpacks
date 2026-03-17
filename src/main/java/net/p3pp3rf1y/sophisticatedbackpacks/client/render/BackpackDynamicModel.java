package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.datafixers.util.Either;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.BlockElement;
import net.minecraft.client.renderer.block.model.BlockModel;
import net.minecraft.client.renderer.block.model.ItemOverrides;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.core.component.PatchedDataComponentMap;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.shapes.BooleanOp;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.neoforged.neoforge.client.ChunkRenderTypeSet;
import net.neoforged.neoforge.client.extensions.common.IClientFluidTypeExtensions;
import net.neoforged.neoforge.client.model.IDynamicBakedModel;
import net.neoforged.neoforge.client.model.IQuadTransformer;
import net.neoforged.neoforge.client.model.data.ModelData;
import net.neoforged.neoforge.client.model.data.ModelProperty;
import net.neoforged.neoforge.client.model.geometry.IGeometryBakingContext;
import net.neoforged.neoforge.client.model.geometry.IGeometryLoader;
import net.neoforged.neoforge.client.model.geometry.IUnbakedGeometry;
import net.neoforged.neoforge.client.model.pipeline.QuadBakingVertexConsumer;
import net.neoforged.neoforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackShapeHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;

import javax.annotation.Nullable;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;

public class BackpackDynamicModel implements IUnbakedGeometry<BackpackDynamicModel> {
	static final ItemDisplayContext WORN = ItemDisplayContext.valueOf("SOPHISTICATEDBACKPACKS_WORN");
	private final Map<ModelPart, ResourceLocation> modelPartParents;

	private BackpackDynamicModel(Map<ModelPart, ResourceLocation> modelPartParents) {
		this.modelPartParents = modelPartParents;
	}

	@Override
	public BakedModel bake(IGeometryBakingContext context, ModelBaker baker, Function<Material, TextureAtlasSprite> spriteGetter, ModelState modelTransform, ItemOverrides overrides) {
		ImmutableMap.Builder<ModelPart, BakedModel> builder = ImmutableMap.builder();
		ImmutableMap.Builder<ModelPart, VoxelShape> shapeBuilder = ImmutableMap.builder();
		modelPartParents.forEach((part, parentModel) -> {
			BlockModel model = createPartModel(context, parentModel);
			model.resolveParents(baker::getModel);
			BakedModel bakedModel = model.bake(baker, spriteGetter, modelTransform);
			if (bakedModel != null) {
				builder.put(part, bakedModel);
			}
			VoxelShape shape = getModelShape(model);
			if (!shape.isEmpty()) {
				shapeBuilder.put(part, shape);
			}
		});
		return new BackpackBakedModel(builder.build(), shapeBuilder.build(), context.getTransforms());
	}

	private static BlockModel createPartModel(IGeometryBakingContext context, ResourceLocation parentModel) {
		ImmutableMap.Builder<String, Either<Material, String>> textures = ImmutableMap.builder();

		if (context.hasMaterial("clips")) {
			textures.put("clips", Either.left(context.getMaterial("clips")));
		}

		return new BlockModel(parentModel, Collections.emptyList(), textures.build(), true, null, ItemTransforms.NO_TRANSFORMS, Collections.emptyList());
	}

	private static VoxelShape getModelShape(BlockModel model) {
		List<BlockElement> elements = model.getElements();
		if (elements.isEmpty()) {
			return Shapes.empty();
		}

		VoxelShape shape = Shapes.empty();
		for (BlockElement element : elements) {
			if (element.rotation != null && Math.abs(element.rotation.angle()) > 0.000001f) {
				return Shapes.empty();
			}
			shape = Shapes.joinUnoptimized(shape, Shapes.box(
					Math.min(element.from.x(), element.to.x()) / 16d,
					Math.min(element.from.y(), element.to.y()) / 16d,
					Math.min(element.from.z(), element.to.z()) / 16d,
					Math.max(element.from.x(), element.to.x()) / 16d,
					Math.max(element.from.y(), element.to.y()) / 16d,
					Math.max(element.from.z(), element.to.z()) / 16d
			), BooleanOp.OR);
		}
		return shape.optimize();
	}

	public static final class BackpackBakedModel implements IDynamicBakedModel {
		private static final ModelProperty<Boolean> HAS_TANK_LEFT = new ModelProperty<>();
		private static final ModelProperty<Boolean> HAS_TANK_RIGHT = new ModelProperty<>();
		private static final ModelProperty<Boolean> HAS_BATTERY = new ModelProperty<>();
		private static final ModelProperty<IRenderedTankUpgrade.TankRenderInfo> LEFT_TANK_RENDER_INFO = new ModelProperty<>();
		private static final ModelProperty<IRenderedTankUpgrade.TankRenderInfo> RIGHT_TANK_RENDER_INFO = new ModelProperty<>();
		private static final ModelProperty<IRenderedBatteryUpgrade.BatteryRenderInfo> BATTERY_RENDER_INFO = new ModelProperty<>();

		private int cachedLeftTankSteps = -1;
		private final Map<FluidCacheKey, List<BakedQuad>> leftTankFluidCache = new HashMap<>();
		private int cachedRightTankSteps = -1;
		private final Map<FluidCacheKey, List<BakedQuad>> rightTankFluidCache = new HashMap<>();
		private int cachedBatterySteps = -1;
		private final Map<Integer, List<BakedQuad>> batteryChargeCache = new HashMap<>();
		private ItemDisplayContext lastContext = ItemDisplayContext.NONE;

		@Override
		public ChunkRenderTypeSet getRenderTypes(BlockState state, RandomSource rand, ModelData data) {
			return ChunkRenderTypeSet.of(RenderType.cutout(), RenderType.translucent());
		}

		@Override
		public List<BakedModel> getRenderPasses(ItemStack itemStack, boolean fabulous) {
			return List.of(tankBakedModel, this);
		}

		private final BackpackItemOverrideList overrideList = new BackpackItemOverrideList(this);
		private final Map<ModelPart, BakedModel> models;
		private final Map<ModelPart, VoxelShape> partShapes;
		private final Map<BackpackShapeHelper.ShapeKey, VoxelShape> shapeCache = new ConcurrentHashMap<>();

		@Nullable
		private AABB leftTankFluidBounds;
		@Nullable
		private AABB rightTankFluidBounds;
		@Nullable
		private AABB batteryChargeBounds;

		private boolean tankLeft;
		@Nullable
		private IRenderedTankUpgrade.TankRenderInfo leftTankRenderInfo = null;
		private boolean tankRight;
		@Nullable
		private IRenderedTankUpgrade.TankRenderInfo rightTankRenderInfo = null;
		private boolean battery;
		@Nullable
		private IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo = null;
		private final ItemTransforms itemTransforms;

		private final TankBakedModel tankBakedModel = new TankBakedModel();

		public BackpackBakedModel(Map<ModelPart, BakedModel> models, Map<ModelPart, VoxelShape> partShapes, ItemTransforms itemTransforms) {
			this.models = models;
			this.partShapes = partShapes;
			this.itemTransforms = itemTransforms;
		}

		public Optional<VoxelShape> getShape(Direction dir, boolean leftTank, boolean rightTank, boolean battery) {
			if (!hasPartShapes()) {
				return Optional.empty();
			}

			BackpackShapeHelper.ShapeKey key = BackpackShapeHelper.ShapeKey.of(dir, leftTank, rightTank, battery);
			return Optional.of(shapeCache.computeIfAbsent(key, k -> composeShape(dir, leftTank, rightTank, battery)));
		}

		private boolean hasPartShapes() {
			return partShapes.containsKey(ModelPart.BASE)
					&& partShapes.containsKey(ModelPart.LEFT_POUCH)
					&& partShapes.containsKey(ModelPart.LEFT_TANK)
					&& partShapes.containsKey(ModelPart.RIGHT_POUCH)
					&& partShapes.containsKey(ModelPart.RIGHT_TANK)
					&& partShapes.containsKey(ModelPart.FRONT_POUCH)
					&& partShapes.containsKey(ModelPart.BATTERY);
		}

		private VoxelShape composeShape(Direction dir, boolean leftTank, boolean rightTank, boolean battery) {
			BackpackShapeHelper.ShapeKey key = BackpackShapeHelper.ShapeKey.of(dir, leftTank, rightTank, battery);
			return BackpackShapeHelper.composeAndRotate(key, this::getPartShape);
		}

		private VoxelShape getPartShape(BackpackShapeHelper.Part part) {
			return switch (part) {
				case BASE -> partShapes.get(ModelPart.BASE);
				case BATTERY -> partShapes.get(ModelPart.BATTERY);
				case FRONT_POUCH -> partShapes.get(ModelPart.FRONT_POUCH);
				case LEFT_POUCH -> partShapes.get(ModelPart.LEFT_POUCH);
				case LEFT_TANK -> partShapes.get(ModelPart.LEFT_TANK);
				case RIGHT_POUCH -> partShapes.get(ModelPart.RIGHT_POUCH);
				case RIGHT_TANK -> partShapes.get(ModelPart.RIGHT_TANK);
			};
		}

		@Nullable
		public BakedQuad getDisplayItemQuad() {
			BakedModel displayItemModel = models.get(ModelPart.DISPLAY_ITEM);
			if (displayItemModel == null) {
				return null;
			}
			List<BakedQuad> quads = displayItemModel.getQuads(null, null, RandomSource.create(), ModelData.EMPTY, null);
			if (quads.isEmpty()) {
				return null;
			}
			return quads.getFirst();
		}

		@Override
		public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData,
										@Nullable RenderType renderType) {
			if (state != null) {
				setPropertiesFromModelData(extraData);
			}

			if (renderType == RenderType.translucent()) {
				return tankBakedModel.getQuads(state, side, rand, extraData, renderType);
			}

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

			if (lastContext != WORN) {
				ret.addAll(models.get(ModelPart.STRAPS).getQuads(state, side, rand, extraData, renderType));
			}

			return ret;
		}

		private void setPropertiesFromModelData(ModelData extraData) {
			tankLeft = extraData.has(HAS_TANK_LEFT) && extraData.get(HAS_TANK_LEFT);
			leftTankRenderInfo = extraData.get(LEFT_TANK_RENDER_INFO);
			tankRight = extraData.has(HAS_TANK_RIGHT) && extraData.get(HAS_TANK_RIGHT);
			rightTankRenderInfo = extraData.get(RIGHT_TANK_RENDER_INFO);
			battery = extraData.has(HAS_BATTERY) && extraData.get(HAS_BATTERY);
			batteryRenderInfo = extraData.get(BATTERY_RENDER_INFO);
		}

		private void addFront(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, List<BakedQuad> ret,
							  boolean battery, @Nullable RenderType renderType) {
			if (battery) {
				if (batteryRenderInfo != null && batteryRenderInfo.getChargeRatio() != 0) {
					float ratio = batteryRenderInfo.getChargeRatio();

					if (cachedBatterySteps < 0) {
						ret.addAll(getBatteryChargeFromModel(state, rand, extraData, renderType, ratio));
					} else {
						int step = ratioToStep(ratio, cachedBatterySteps);
						ret.addAll(batteryChargeCache.computeIfAbsent(step,
								s -> getBatteryChargeFromModel(state, rand, extraData, renderType, stepToRatio(s, cachedBatterySteps))));
					}
				}
				ret.addAll(models.get(ModelPart.BATTERY).getQuads(state, side, rand, extraData, renderType));
			} else {
				ret.addAll(models.get(ModelPart.FRONT_POUCH).getQuads(state, side, rand, extraData, renderType));
			}
		}

		private List<BakedQuad> getBatteryChargeFromModel(@Nullable BlockState state, RandomSource rand, ModelData extraData,
														  @Nullable RenderType renderType, float chargeRatio) {
			BakedModel chargeModel = models.get(ModelPart.BATTERY_CHARGE);
			if (chargeModel == null) {
				return Collections.emptyList();
			}

			List<BakedQuad> src = getAllQuads(chargeModel, state, rand, extraData, renderType);
			if (src.isEmpty()) {
				return Collections.emptyList();
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
				return Collections.emptyList();
			}

			int step = ratioToStep(chargeRatio, cachedBatterySteps);
			if (step <= 0) {
				return Collections.emptyList();
			}
			if (step >= cachedBatterySteps) {
				step = cachedBatterySteps;
			}

			// Avoid exact 1.0 slice for full charge to prevent edge-case clipping/z-fighting artifacts on custom pack quads.
			float stepRatio = step >= cachedBatterySteps ? 0.9999f : stepToRatio(step, cachedBatterySteps);

			SliceSpec s = horizontalSliceSpecFromUv(src, bounds, stepRatio, batteryFillAxis);
			return sliceQuadsAxis(src, s.axis(), s.cut(), s.keepGreaterOrEqual());
		}

		private record SliceSpec(Direction.Axis axis, double cut, boolean keepGreaterOrEqual) {}

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
				int[] v = q.getVertices();
				int stride = v.length / 4;
				for (int i = 0; i < 4; i++) {
					int base = i * stride;
					float x = Float.intBitsToFloat(v[base]);
					float z = Float.intBitsToFloat(v[base + 2]);
					float u = Float.intBitsToFloat(v[base + 4]);
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

		private void addRightSide(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, List<BakedQuad> ret,
								  boolean tankRight, @Nullable RenderType renderType) {
			if (tankRight) {
				ret.addAll(models.get(ModelPart.RIGHT_TANK).getQuads(state, side, rand, extraData, renderType));
			} else {
				ret.addAll(models.get(ModelPart.RIGHT_POUCH).getQuads(state, side, rand, extraData, renderType));
			}
		}

		private void addLeftSide(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, List<BakedQuad> ret,
								 boolean tankLeft, @Nullable RenderType renderType) {
			if (tankLeft) {
				ret.addAll(models.get(ModelPart.LEFT_TANK).getQuads(state, side, rand, extraData, renderType));
			} else {
				ret.addAll(models.get(ModelPart.LEFT_POUCH).getQuads(state, side, rand, extraData, renderType));
			}
		}

		private record FluidCacheKey(Fluid fluid, PatchedDataComponentMap components, int step) {
		}

		private List<BakedQuad> getTankFluidFromModel(@Nullable BlockState state, RandomSource rand, ModelData extraData, @Nullable RenderType renderType,
													  ModelPart fluidPart, FluidStack fluidStack, float ratio, boolean isLeft) {

			if (fluidStack == FluidStack.EMPTY || Mth.equal(ratio, 0f)) {
				return Collections.emptyList();
			}

			BakedModel fluidModel = models.get(fluidPart);
			if (fluidModel == null) {
				return Collections.emptyList();
			}

			List<BakedQuad> src = getAllQuads(fluidModel, state, rand, extraData, renderType);
			if (src.isEmpty()) {
				return Collections.emptyList();
			}

			int steps = isLeft ? cachedLeftTankSteps : cachedRightTankSteps;
			if (steps < 0) {
				steps = computeStepsFromModelUV(src, Direction.Axis.Y);
				if (isLeft) cachedLeftTankSteps = steps;
				else cachedRightTankSteps = steps;
			}

			AABB cached = isLeft ? leftTankFluidBounds : rightTankFluidBounds;
			AABB max = cached != null ? cached : computeBoundsFromQuads(src);
			if (isLeft) {
				leftTankFluidBounds = max;
			} else {
				rightTankFluidBounds = max;
			}
			if (max == null) {
				return Collections.emptyList();
			}

			int step = ratioToStep(ratio, steps);
			if (step <= 0) {
				return Collections.emptyList();
			}
			if (step >= steps) {
				step = steps;
			}

			float stepRatio = stepToRatio(step, steps);

			double cut = max.minY + (max.maxY - max.minY) * stepRatio;

			List<BakedQuad> sliced = sliceQuadsAxis(src, Direction.Axis.Y, cut, false);
			if (sliced.isEmpty()) {
				return Collections.emptyList();
			}

			IClientFluidTypeExtensions props = IClientFluidTypeExtensions.of(fluidStack.getFluid());
			ResourceLocation stillTex = props.getStillTexture(fluidStack);
			TextureAtlasSprite newSprite = Minecraft.getInstance().getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(stillTex);
			int argb = props.getTintColor(fluidStack);

			List<BakedQuad> ret = respriteAndTintQuads(sliced, newSprite, argb);

			BakedQuad top = buildFluidTopQuad((float) cut, max, src, newSprite, argb);
			if (top != null) {
				ret.add(top);
			}

			return ret;
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
			qb.setSprite(sprite);
			qb.setDirection(Direction.UP);
			qb.setTintIndex(-1);
			Vec3i n = Direction.UP.getNormal();

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
				Direction dir = q.getDirection();
				if (dir != d1 && dir != d2) {
					continue;
				}

				int[] v = q.getVertices();
				int stride = v.length / 4;

				float minCoord = Float.POSITIVE_INFINITY;
				float maxCoord = Float.NEGATIVE_INFINITY;
				float minU = Float.POSITIVE_INFINITY;
				float maxU = Float.NEGATIVE_INFINITY;

				for (int i = 0; i < 4; i++) {
					int base = i * stride;
					float x = Float.intBitsToFloat(v[base]);
					float y = Float.intBitsToFloat(v[base + 1]);
					float z = Float.intBitsToFloat(v[base + 2]);
					float u = Float.intBitsToFloat(v[base + 4]);

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

				TextureAtlasSprite s = q.getSprite();
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

		private static List<BakedQuad> getAllQuads(BakedModel m,
												   @Nullable BlockState state,
												   RandomSource rand, ModelData extraData,
												   @Nullable RenderType renderType) {
			List<BakedQuad> all = m.getQuads(state, null, rand, extraData, renderType);
			if (!all.isEmpty()) {
				return all;
			}
			List<BakedQuad> ret = Lists.newArrayList();
			for (Direction d : Direction.values()) {
				ret.addAll(m.getQuads(state, d, rand, extraData, renderType));
			}
			return ret;
		}

		@Nullable
		private static AABB computeBoundsFromQuads(List<BakedQuad> quads) {
			if (quads.isEmpty()) {
				return null;
			}
			float minX = Float.POSITIVE_INFINITY, minY = Float.POSITIVE_INFINITY, minZ = Float.POSITIVE_INFINITY;
			float maxX = Float.NEGATIVE_INFINITY, maxY = Float.NEGATIVE_INFINITY, maxZ = Float.NEGATIVE_INFINITY;

			for (BakedQuad q : quads) {
				int[] v = q.getVertices();
				final int stride = v.length / 4;
				for (int i = 0; i < 4; i++) {
					int base = i * stride;
					float x = Float.intBitsToFloat(v[base]);
					float y = Float.intBitsToFloat(v[base + 1]);
					float z = Float.intBitsToFloat(v[base + 2]);
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

		private static List<BakedQuad> sliceQuadsAxis(List<BakedQuad> src, Direction.Axis axis, double cut, boolean keepGreaterOrEqual) {
			List<BakedQuad> out = new ArrayList<>(src.size());
			for (BakedQuad q : src) {
				BakedQuad sliced = sliceQuadAxis(q, axis, (float) cut, keepGreaterOrEqual);
				if (sliced != null) {
					out.add(sliced);
				}
			}
			return out;
		}

		@Nullable
		private static BakedQuad sliceQuadAxis(BakedQuad q, Direction.Axis axis, float cut, boolean keepGreaterOrEqual) {
			int[] v = q.getVertices();
			int stride = v.length / 4;

			Vert[] in = new Vert[4];
			for (int i = 0; i < 4; i++) {
				int base = i * stride;
				in[i] = new Vert(
						Float.intBitsToFloat(v[base]),
						Float.intBitsToFloat(v[base + 1]),
						Float.intBitsToFloat(v[base + 2]),
						Float.intBitsToFloat(v[base + 4]),
						Float.intBitsToFloat(v[base + 5])
				);
			}

			List<Vert> out = clipAgainstPlane(Arrays.asList(in), axis, cut, keepGreaterOrEqual);

			if (out.isEmpty()) return null;

			while (out.size() < 4) {
				out.add(out.get(out.size() - 1));
			}
			if (out.size() > 4) {
				out = out.subList(0, 4);
			}

			QuadBakingVertexConsumer qb = new QuadBakingVertexConsumer();
			qb.setSprite(q.getSprite());
			qb.setDirection(q.getDirection());
			qb.setTintIndex(q.getTintIndex());
			Vec3i n = q.getDirection().getNormal();

			for (int i = 0; i < 4; i++) {
				Vert p = out.get(i);
				qb.addVertex(p.x, p.y, p.z)
						.setColor(1f, 1f, 1f, 1f)
						.setUv(p.u, p.v)
						.setNormal(n.getX(), n.getY(), n.getZ());
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

			return new Vert(
					Mth.lerp(t, a.x, b.x),
					Mth.lerp(t, a.y, b.y),
					Mth.lerp(t, a.z, b.z),
					Mth.lerp(t, a.u, b.u),
					Mth.lerp(t, a.v, b.v)
			);
		}

		private record Vert(float x, float y, float z, float u, float v) {
		}

		private static List<BakedQuad> respriteAndTintQuads(List<BakedQuad> src, TextureAtlasSprite newSprite, int argb) {
			float a = (argb >>> 24 & 0xFF) / 255f;
			float r = (argb >>> 16 & 0xFF) / 255f;
			float g = (argb >>> 8 & 0xFF) / 255f;
			float b = (argb & 0xFF) / 255f;
			float[] cols = new float[]{a, r, g, b};
			List<BakedQuad> out = new ArrayList<>(src.size());
			for (BakedQuad q : src) {
				out.add(respriteAndTintQuad(q, newSprite, cols));
			}
			return out;
		}

		private static BakedQuad respriteAndTintQuad(BakedQuad q, TextureAtlasSprite newSprite, float[] cols) {
			TextureAtlasSprite oldSprite = q.getSprite();
			int[] in = q.getVertices();

			QuadBakingVertexConsumer qb = new QuadBakingVertexConsumer();
			qb.setSprite(newSprite);
			qb.setDirection(q.getDirection());
			qb.setTintIndex(-1);

			Vec3i n = q.getDirection().getNormal();

			for (int vi = 0; vi < 4; vi++) {
				int base = vi * IQuadTransformer.STRIDE;

				float x = Float.intBitsToFloat(in[base + IQuadTransformer.POSITION]);
				float y = Float.intBitsToFloat(in[base + IQuadTransformer.POSITION + 1]);
				float z = Float.intBitsToFloat(in[base + IQuadTransformer.POSITION + 2]);

				float uOld = Float.intBitsToFloat(in[base + IQuadTransformer.UV0]);
				float vOld = Float.intBitsToFloat(in[base + IQuadTransformer.UV0 + 1]);

				float uNew = remapU(oldSprite, newSprite, uOld);
				float vNew = remapV(oldSprite, newSprite, vOld);

				int packedUv2 = in[base + IQuadTransformer.UV2];
				int lightU = packedUv2 & 0xFFFF;
				int lightV = (packedUv2 >>> 16) & 0xFFFF;

				qb.addVertex(x, y, z)
						.setColor(cols[1], cols[2], cols[3], cols[0])
						.setUv(uNew, vNew)
						.setUv2(lightU, lightV)
						.setNormal(n.getX(), n.getY(), n.getZ());

				if (IQuadTransformer.UV1 >= 0) {
					int packedUv1 = in[base + IQuadTransformer.UV1];
					int ovU = packedUv1 & 0xFFFF;
					int ovV = (packedUv1 >>> 16) & 0xFFFF;
					qb.setUv1(ovU, ovV);
				}
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
			if (quads.isEmpty()) return 0;

			double bestModelSpan = -1;
			double bestPixelSpan = -1;

			for (BakedQuad q : quads) {
				if (fillAxis == Direction.Axis.Y && (q.getDirection() == Direction.UP || q.getDirection() == Direction.DOWN)) {
					continue;
				}

				int[] v = q.getVertices();
				int stride = v.length / 4;

				float minCoord = Float.POSITIVE_INFINITY;
				float maxCoord = Float.NEGATIVE_INFINITY;

				float minU = Float.POSITIVE_INFINITY, maxU = Float.NEGATIVE_INFINITY;
				float minV = Float.POSITIVE_INFINITY, maxV = Float.NEGATIVE_INFINITY;

				for (int i = 0; i < 4; i++) {
					int base = i * stride;
					float x = Float.intBitsToFloat(v[base]);
					float y = Float.intBitsToFloat(v[base + 1]);
					float z = Float.intBitsToFloat(v[base + 2]);

					float u = Float.intBitsToFloat(v[base + 4]);
					float vv = Float.intBitsToFloat(v[base + 5]);

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
				if (modelSpan <= 1e-6) continue;

				TextureAtlasSprite s = q.getSprite();

				double pixelSpan;
				if (fillAxis == Direction.Axis.X) {
					double denom = (s.getU1() - s.getU0());
					if (Math.abs(denom) < 1e-9) continue;
					double uNormSpan = Math.abs(maxU - minU) / denom;
					int texW = s.contents().width();
					pixelSpan = uNormSpan * texW;
				} else if (fillAxis == Direction.Axis.Y) {
					double denom = (s.getV1() - s.getV0());
					if (Math.abs(denom) < 1e-9) continue;
					double vNormSpan = Math.abs(maxV - minV) / denom;
					int texH = s.contents().height();
					pixelSpan = vNormSpan * texH;
				} else {
					double denom = (s.getU1() - s.getU0());
					if (Math.abs(denom) < 1e-9) continue;
					double uNormSpan = Math.abs(maxU - minU) / denom;
					int texW = s.contents().width();
					pixelSpan = uNormSpan * texW;
				}

				if (modelSpan > bestModelSpan) {
					bestModelSpan = modelSpan;
					bestPixelSpan = pixelSpan;
				}
			}

			if (bestPixelSpan <= 0) return 0;

			return Mth.clamp((int) Math.round(bestPixelSpan), 1, 64);
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

		@SuppressWarnings("java:S1874")
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
		public BakedModel applyTransform(ItemDisplayContext transformType, PoseStack poseStack, boolean applyLeftHandTransform) {
			if (transformType == ItemDisplayContext.NONE) {
				return this;
			}

			lastContext = transformType;
			itemTransforms.getTransform(transformType).apply(applyLeftHandTransform, poseStack);

			return this;
		}

		@SuppressWarnings("deprecation")
		@Override
		public ItemTransforms getTransforms() {
			return itemTransforms;
		}

		@Override
		public ModelData getModelData(BlockAndTintGetter level, BlockPos pos, BlockState state, ModelData modelData) {
			return level.getBlockEntity(pos, ModBlocks.BACKPACK_TILE_TYPE.get()).map(backpackBlockEntity -> {
				ModelData.Builder modelDataBuilder = modelData.derive();
				RenderInfo renderInfo = backpackBlockEntity.getBackpackWrapper().getRenderInfo();
				Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> tankRenderInfos = renderInfo.getTankRenderInfos();
				tankRenderInfos.forEach((tankPos, tankInfo) -> {
					if (tankPos == TankPosition.LEFT) {
						modelDataBuilder.with(HAS_TANK_LEFT, true);
						modelDataBuilder.with(LEFT_TANK_RENDER_INFO, tankInfo);
					} else {
						modelDataBuilder.with(HAS_TANK_RIGHT, true);
						modelDataBuilder.with(RIGHT_TANK_RENDER_INFO, tankInfo);
					}
				});
				renderInfo.getBatteryRenderInfo().ifPresent(batteryInfo -> {
					modelDataBuilder.with(HAS_BATTERY, true);
					modelDataBuilder.with(BATTERY_RENDER_INFO, batteryInfo);
				});
				return modelDataBuilder.build();
			}).orElse(modelData);
		}

		private class TankBakedModel implements IDynamicBakedModel {
			@Override
			public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, @Nullable RenderType renderType) {
				if (side != null) {
					return Collections.emptyList();
				}
				List<BakedQuad> ret = new ArrayList<>();

				boolean left = state == null ? tankLeft : state.getValue(LEFT_TANK);
				boolean right = state == null ? tankRight : state.getValue(RIGHT_TANK);
				IRenderedTankUpgrade.TankRenderInfo leftTankRenderInfo = BackpackBakedModel.this.leftTankRenderInfo;
				IRenderedTankUpgrade.TankRenderInfo rightTankRenderInfo = BackpackBakedModel.this.rightTankRenderInfo;

				if (left && leftTankRenderInfo != null && leftTankRenderInfo.getFillRatio() != 0) {
					leftTankRenderInfo.getFluid().ifPresent(fluid -> {
						if (cachedLeftTankSteps != -1) {
							FluidCacheKey cacheKey = getFluidCacheKey(fluid, cachedLeftTankSteps, leftTankRenderInfo.getFillRatio());
							if (leftTankFluidCache.containsKey(cacheKey)) {
								ret.addAll(leftTankFluidCache.get(cacheKey));
								return;
							}
						}
						List<BakedQuad> fluidQuads = getTankFluidFromModel(state, rand, extraData, renderType, ModelPart.LEFT_TANK_FLUID, fluid, leftTankRenderInfo.getFillRatio(), true);
						leftTankFluidCache.put(getFluidCacheKey(fluid, cachedLeftTankSteps, leftTankRenderInfo.getFillRatio()), fluidQuads);
						ret.addAll(fluidQuads);
					});
				}
				if (right && rightTankRenderInfo != null && rightTankRenderInfo.getFillRatio() != 0) {
					rightTankRenderInfo.getFluid().ifPresent(fluid -> {
						if (cachedRightTankSteps != -1) {
							FluidCacheKey cacheKey = getFluidCacheKey(fluid, cachedRightTankSteps, rightTankRenderInfo.getFillRatio());
							if (rightTankFluidCache.containsKey(cacheKey)) {
								ret.addAll(rightTankFluidCache.get(cacheKey));
								return;
							}
						}
						List<BakedQuad> fluidQuads = getTankFluidFromModel(state, rand, extraData, renderType, ModelPart.RIGHT_TANK_FLUID, fluid, rightTankRenderInfo.getFillRatio(), false);
						rightTankFluidCache.put(getFluidCacheKey(fluid, cachedRightTankSteps, rightTankRenderInfo.getFillRatio()), fluidQuads);
						ret.addAll(fluidQuads);
					});
				}

				return ret;
			}

			private FluidCacheKey getFluidCacheKey(FluidStack fluid, int cachedSteps, float fillRatio) {
				int step = cachedSteps > 0 ? ratioToStep(fillRatio, cachedSteps) : 0;
				return new FluidCacheKey(fluid.getFluid(), fluid.getComponents(), step);
			}

			@Override
			public List<RenderType> getRenderTypes(ItemStack itemStack, boolean fabulous) {
				return List.of(RenderType.translucent());
			}

			@Override
			public boolean useAmbientOcclusion() {
				return BackpackBakedModel.this.useAmbientOcclusion();
			}

			@Override
			public boolean isGui3d() {
				return BackpackBakedModel.this.isGui3d();
			}

			@Override
			public boolean usesBlockLight() {
				return BackpackBakedModel.this.usesBlockLight();
			}

			@Override
			public boolean isCustomRenderer() {
				return BackpackBakedModel.this.isCustomRenderer();
			}

			@Override
			public TextureAtlasSprite getParticleIcon() {
				return BackpackBakedModel.this.getParticleIcon();
			}

			@Override
			public ItemOverrides getOverrides() {
				return BackpackBakedModel.this.getOverrides();
			}
		}

	}

	private static class BackpackItemOverrideList extends ItemOverrides {
		private final BackpackBakedModel backpackModel;

		public BackpackItemOverrideList(BackpackBakedModel backpackModel) {
			this.backpackModel = backpackModel;
		}

		@Nullable
		@Override
		public BakedModel resolve(BakedModel model, ItemStack stack, @Nullable ClientLevel world, @Nullable LivingEntity livingEntity, int seed) {
			backpackModel.tankRight = false;
			backpackModel.tankLeft = false;
			backpackModel.rightTankRenderInfo = null;
			backpackModel.leftTankRenderInfo = null;
			backpackModel.battery = false;
			backpackModel.batteryRenderInfo = null;
			IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
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

			return backpackModel;
		}
	}

	public static final class Loader implements IGeometryLoader<BackpackDynamicModel> {
		public static final Loader INSTANCE = new Loader();

		@Override
		public BackpackDynamicModel read(JsonObject modelContents, JsonDeserializationContext deserializationContext) {
			ImmutableMap.Builder<ModelPart, ResourceLocation> builder = ImmutableMap.builder();
			for (ModelPart part : ModelPart.values()) {
				builder.put(part, SophisticatedBackpacks.getRL("block/backpack_" + part.name().toLowerCase(Locale.ENGLISH)));
			}
			return new BackpackDynamicModel(builder.build());
		}
	}

	private enum ModelPart {
		BASE,
		BATTERY,
		FRONT_POUCH,
		LEFT_POUCH,
		LEFT_TANK,
		RIGHT_POUCH,
		RIGHT_TANK,
		STRAPS,
		LEFT_TANK_FLUID,
		RIGHT_TANK_FLUID,
		BATTERY_CHARGE,
		DISPLAY_ITEM
	}
}
