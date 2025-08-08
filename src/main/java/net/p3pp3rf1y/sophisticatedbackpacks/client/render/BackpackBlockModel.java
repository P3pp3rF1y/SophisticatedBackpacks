package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.*;
import net.minecraft.client.renderer.chunk.ChunkSectionLayer;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.util.RandomSource;
import net.minecraft.util.context.ContextMap;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.neoforged.neoforge.client.extensions.common.IClientFluidTypeExtensions;
import net.neoforged.neoforge.client.model.DynamicBlockStateModel;
import net.neoforged.neoforge.client.model.UnbakedModelLoader;
import net.neoforged.neoforge.client.model.block.CustomUnbakedBlockStateModel;
import net.neoforged.neoforge.client.model.pipeline.QuadBakingVertexConsumer;
import net.neoforged.neoforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;
import org.joml.Matrix4fc;
import org.joml.Vector3f;
import org.joml.Vector4f;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;

public class BackpackBlockModel implements UnbakedModel {
	private final Map<ModelPart, UnbakedModel> modelParts;

	private BackpackBlockModel(Map<ModelPart, UnbakedModel> modelParts) {
		this.modelParts = modelParts;
	}

	public BlockStateModel bakeBlockStateModel(ModelBaker baker, ResolvedModel resolvedModel, ModelState modelState) {
		ImmutableMap.Builder<ModelPart, QuadCollection> builder = ImmutableMap.builder();
		modelParts.forEach((part, model) -> {
			//noinspection DataFlowIssue - the model is constructed in the Loader class below and will always have parent
			builder.put(part, baker.getModel(model.parent()).getTopGeometry().bake(getTextureSlots(baker, model, resolvedModel), baker, modelState, resolvedModel, ContextMap.EMPTY));
		});
		return new BlockStateModel(builder.build(), modelState, resolvedModel.resolveParticleSprite(getTextureSlots(baker, modelParts.get(ModelPart.BASE), resolvedModel), baker));
	}

	private TextureSlots getTextureSlots(ModelBaker baker, UnbakedModel partModel, ModelDebugName debugName) {
		TextureSlots.Resolver resolver = new TextureSlots.Resolver();

		resolver.addLast(partModel.textureSlots());

		@Nullable
		ResourceLocation parent = partModel.parent();
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
	public void resolveDependencies(Resolver resolver) {
		modelParts.values().forEach(model -> {
			ResourceLocation parent = model.parent();
			if (parent != null) {
				resolver.markDependency(parent);
			}
			model.resolveDependencies(resolver);
		});
	}

	public static final class BlockStateModel implements DynamicBlockStateModel {
		private static final ResourceLocation BACKPACK_MODULES_TEXTURE = ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "block/backpack_modules");

		private final Map<ModelPart, QuadCollection> models;
		private final ModelState modelState;
		private final TextureAtlasSprite particleIcon;

		public boolean tankLeft;
		@Nullable
		public IRenderedTankUpgrade.TankRenderInfo leftTankRenderInfo = null;
		public boolean tankRight;
		@Nullable
		public IRenderedTankUpgrade.TankRenderInfo rightTankRenderInfo = null;
		public boolean battery;
		@Nullable
		public IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo = null;

		public BlockStateModel(Map<ModelPart, QuadCollection> models, ModelState modelState, TextureAtlasSprite particleIcon) {
			this.models = models;
			this.modelState = modelState;
			this.particleIcon = particleIcon;
		}

		public void collectParts(@Nullable BlockAndTintGetter level, BlockPos pos, @Nullable BlockState state, RandomSource rand, List<BlockModelPart> parts) {
			if (state != null) {
				tankLeft = state.getValue(LEFT_TANK);
				tankRight = state.getValue(RIGHT_TANK);
				battery = state.getValue(BATTERY);
			}

			collectPartsNoStateUpdate(parts);
		}

		private void collectPartsNoStateUpdate(List<BlockModelPart> parts) {
			QuadCollection.Builder builder = new QuadCollection.Builder();
			builder.addAll(models.get(ModelPart.BASE));
			addLeftSide(builder);
			addRightSide(builder);
			addFront(builder);

			parts.add(new SimpleModelWrapper(builder.build(), true, particleIcon, ChunkSectionLayer.CUTOUT));
		}

		private void addFront(QuadCollection.Builder builder) {
			if (battery) {
				if (batteryRenderInfo != null) {
					addCharge(builder, batteryRenderInfo.getChargeRatio());
				}
				builder.addAll(models.get(ModelPart.BATTERY));
			} else {
				builder.addAll(models.get(ModelPart.FRONT_POUCH));
			}
		}

		private void addCharge(QuadCollection.Builder builder, float chargeRatio) {
			if (Mth.equal(chargeRatio, 0)) {
				return;
			}
			int pixels = (int) (chargeRatio * 4);
			float minX = (10 - pixels) / 16f;
			float minY = 2 / 16f;
			float minZ = 1.95f / 16f;
			float maxX = minX + pixels / 16f;
			float maxY = minY + 1 / 16f;
			float[] cols = new float[]{1f, 1f, 1f, 1f};
			TextureAtlasSprite sprite = Minecraft.getInstance().getTextureAtlas(TextureAtlas.LOCATION_BLOCKS).apply(BACKPACK_MODULES_TEXTURE);
			builder.addUnculledFace(createQuad(List.of(getVector(maxX, maxY, minZ), getVector(maxX, minY, minZ), getVector(minX, minY, minZ), getVector(minX, maxY, minZ)), cols, sprite, Direction.NORTH, 14, 14 + (pixels / 2f), 6, 6.5f));
		}

		private void addRightSide(QuadCollection.Builder builder) {
			if (tankRight) {
				if (rightTankRenderInfo != null) {
					rightTankRenderInfo.getFluid().ifPresent(fluid -> addFluid(builder, fluid, rightTankRenderInfo.getFillRatio(), 0.6 / 16d));
				}
				builder.addAll(models.get(ModelPart.RIGHT_TANK));
			} else {
				builder.addAll(models.get(ModelPart.RIGHT_POUCH));
			}
		}

		private void addLeftSide(QuadCollection.Builder builder) {
			if (tankLeft) {
				if (leftTankRenderInfo != null) {
					leftTankRenderInfo.getFluid().ifPresent(fluid -> addFluid(builder, fluid, leftTankRenderInfo.getFillRatio(), 12.85 / 16d));
				}
				builder.addAll(models.get(ModelPart.LEFT_TANK));
			} else {
				builder.addAll(models.get(ModelPart.LEFT_POUCH));
			}
		}

		private void addFluid(QuadCollection.Builder builder, FluidStack fluidStack, float ratio, double xMin) {
			if (fluidStack == FluidStack.EMPTY || Mth.equal(ratio, 0.0f)) {
				return;
			}

			double yMin = 1.5 / 16d;
			double yMax = yMin + (ratio * 6) / 16d;
			AABB bounds = new AABB(xMin, yMin, 6.75 / 16d, xMin + 2.5 / 16d, yMax, 9.25 / 16d);

			IClientFluidTypeExtensions renderProperties = IClientFluidTypeExtensions.of(fluidStack.getFluid());
			ResourceLocation texture = renderProperties.getStillTexture(fluidStack);
			int color = renderProperties.getTintColor(fluidStack);
			float[] cols = new float[]{(color >> 24 & 0xFF) / 255F, (color >> 16 & 0xFF) / 255F, (color >> 8 & 0xFF) / 255F, (color & 0xFF) / 255F};
			TextureAtlasSprite still = Minecraft.getInstance().getTextureAtlas(TextureAtlas.LOCATION_BLOCKS).apply(texture);
			float bx1 = 0;
			float bx2 = 5;
			float by1 = 0;
			float by2 = ratio * 10;
			float bz1 = 0;
			float bz2 = 5;

			builder.addUnculledFace(createQuad(List.of(getVector(bounds.minX, bounds.maxY, bounds.minZ), getVector(bounds.minX, bounds.maxY, bounds.maxZ), getVector(bounds.maxX, bounds.maxY, bounds.maxZ), getVector(bounds.maxX, bounds.maxY, bounds.minZ)), cols, still, Direction.UP, bx1, bx2, bz1, bz2));
			builder.addUnculledFace(createQuad(List.of(getVector(bounds.maxX, bounds.maxY, bounds.minZ), getVector(bounds.maxX, bounds.minY, bounds.minZ), getVector(bounds.minX, bounds.minY, bounds.minZ), getVector(bounds.minX, bounds.maxY, bounds.minZ)), cols, still, Direction.NORTH, bx1, bx2, by1, by2));
			builder.addUnculledFace(createQuad(List.of(getVector(bounds.minX, bounds.maxY, bounds.maxZ), getVector(bounds.minX, bounds.minY, bounds.maxZ), getVector(bounds.maxX, bounds.minY, bounds.maxZ), getVector(bounds.maxX, bounds.maxY, bounds.maxZ)), cols, still, Direction.SOUTH, bx1, bx2, by1, by2));
			builder.addUnculledFace(createQuad(List.of(getVector(bounds.minX, bounds.maxY, bounds.minZ), getVector(bounds.minX, bounds.minY, bounds.minZ), getVector(bounds.minX, bounds.minY, bounds.maxZ), getVector(bounds.minX, bounds.maxY, bounds.maxZ)), cols, still, Direction.WEST, bz1, bz2, by1, by2));
			builder.addUnculledFace(createQuad(List.of(getVector(bounds.maxX, bounds.maxY, bounds.maxZ), getVector(bounds.maxX, bounds.minY, bounds.maxZ), getVector(bounds.maxX, bounds.minY, bounds.minZ), getVector(bounds.maxX, bounds.maxY, bounds.minZ)), cols, still, Direction.EAST, bz1, bz2, by1, by2));
		}

		private Vector3f getVector(double x, double y, double z) {
			Vector3f ret = new Vector3f((float) x, (float) y, (float) z);
			rotate(ret, modelState.transformation().getMatrix());
			return ret;
		}

		private BakedQuad createQuad(List<Vector3f> vecs, float[] colors, TextureAtlasSprite sprite, Direction face, float u1, float u2, float v1, float v2) {
			QuadBakingVertexConsumer quadBaker = new QuadBakingVertexConsumer();
			quadBaker.setSprite(sprite);
			Vec3i dirVec = face.getUnitVec3i();
			quadBaker.setDirection(face);
			quadBaker.setTintIndex(-1);

			u1 = sprite.getU0() + u1 / 4f * sprite.uvShrinkRatio();
			u2 = sprite.getU0() + u2 / 4f * sprite.uvShrinkRatio();

			v1 = sprite.getV0() + v1 / 4f * sprite.uvShrinkRatio();
			v2 = sprite.getV0() + v2 / 4f * sprite.uvShrinkRatio();

			quadBaker.addVertex(vecs.get(0).x(), vecs.get(0).y(), vecs.get(0).z()).setColor(colors[1], colors[2], colors[3], colors[0]).setUv(u1, v1).setNormal(dirVec.getX(), dirVec.getY(), dirVec.getZ());
			quadBaker.addVertex(vecs.get(1).x(), vecs.get(1).y(), vecs.get(1).z()).setColor(colors[1], colors[2], colors[3], colors[0]).setUv(u1, v2).setNormal(dirVec.getX(), dirVec.getY(), dirVec.getZ());
			quadBaker.addVertex(vecs.get(2).x(), vecs.get(2).y(), vecs.get(2).z()).setColor(colors[1], colors[2], colors[3], colors[0]).setUv(u2, v2).setNormal(dirVec.getX(), dirVec.getY(), dirVec.getZ());
			quadBaker.addVertex(vecs.get(3).x(), vecs.get(3).y(), vecs.get(3).z()).setColor(colors[1], colors[2], colors[3], colors[0]).setUv(u2, v1).setNormal(dirVec.getX(), dirVec.getY(), dirVec.getZ());
			return quadBaker.bakeQuad();
		}

		private void rotate(Vector3f posIn, Matrix4fc transform) {
			Vector3f originIn = new Vector3f(0.5f, 0.5f, 0.5f);
			Vector4f vector4f = transform.transform(new Vector4f(posIn.x() - originIn.x(), posIn.y() - originIn.y(), posIn.z() - originIn.z(), 1.0F));
			posIn.set(vector4f.x() + originIn.x(), vector4f.y() + originIn.y(), vector4f.z() + originIn.z());
		}

		@Override
		public TextureAtlasSprite particleIcon() {
			return particleIcon;
		}

		public List<BakedQuad> getQuads() {
			List<BlockModelPart> parts = new ArrayList<>();
			collectPartsNoStateUpdate(parts);

			List<BakedQuad> bakedQuads = new ArrayList<>();

			for (BlockModelPart part : parts) {
				for (Direction dir : Direction.values()) {
					bakedQuads.addAll(part.getQuads(dir));
				}
				bakedQuads.addAll(part.getQuads(null));
			}

			return bakedQuads;
		}
	}

	public record UnbakedBlockStateModel(Variant variant) implements CustomUnbakedBlockStateModel {
		public static final MapCodec<UnbakedBlockStateModel> CODEC = RecordCodecBuilder.mapCodec(instance ->
				instance.group(Variant.MAP_CODEC.forGetter(UnbakedBlockStateModel::variant)).apply(instance, UnbakedBlockStateModel::new));
		public static final ResourceLocation ID = SophisticatedBackpacks.getRL("backpack_model_loader");

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

			TextureSlots.Data.Builder texturesBuilder = new TextureSlots.Data.Builder();
			if (modelContents.has("clipsTexture")) {
				ResourceLocation clipsTexture = ResourceLocation.tryParse(modelContents.get("clipsTexture").getAsString());
				if (clipsTexture != null) {
					texturesBuilder.addTexture("clips", new Material(TextureAtlas.LOCATION_BLOCKS, clipsTexture));
				}
			}
			for (ModelPart part : ModelPart.values()) {
				addPartModel(builder, part, texturesBuilder.build());
			}
			return new BackpackBlockModel(builder.build());
		}

		private void addPartModel(ImmutableMap.Builder<ModelPart, UnbakedModel> builder, ModelPart modelPart, TextureSlots.Data textures) {
			builder.put(modelPart, new BlockModel(null, null, true, ItemTransforms.NO_TRANSFORMS, textures, SophisticatedBackpacks.getRL("block/backpack_" + modelPart.name().toLowerCase(Locale.ENGLISH))));
		}
	}

	public enum ModelPart {
		BASE,
		BATTERY,
		FRONT_POUCH,
		LEFT_POUCH,
		LEFT_TANK,
		RIGHT_POUCH,
		RIGHT_TANK
	}
}
