package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.base.Suppliers;
import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.client.Minecraft;
import net.minecraft.client.color.item.ItemTintSource;
import net.minecraft.client.color.item.ItemTintSources;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.Sheets;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.ItemTransform;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.item.BlockModelWrapper;
import net.minecraft.client.renderer.item.ItemModel;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.client.renderer.special.NoDataSpecialModelRenderer;
import net.minecraft.client.resources.model.BlockModelRotation;
import net.minecraft.client.resources.model.ResolvedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;
import org.joml.Vector3f;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

public class BackpackItemModel implements ItemModel {
	public static final Vector3f DEFAULT_ROTATION = new Vector3f(0.0F, 0.0F, 0.0F);
	private static final ItemTransforms ITEM_TRANSFORMS = createItemTransforms();

	@SuppressWarnings("java:S4738")
	//ItemTransforms require Guava ImmutableMap to be passed in so no way to change that to java Map
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


	private final BackpackBlockModel.BlockStateModel baseModel;
	private final List<ItemTintSource> tints;
	private final Supplier<Vector3f[]> extents;

	public BackpackItemModel(BackpackBlockModel.BlockStateModel baseModel, List<ItemTintSource> tints) {
		this.baseModel = baseModel;
		this.tints = tints;
		extents = Suppliers.memoize(() -> BlockModelWrapper.computeExtents(baseModel.getQuads()));
	}

	@Override
	public void update(ItemStackRenderState stackRenderState, ItemStack stack, ItemModelResolver itemModelResolver, ItemDisplayContext displayContext, @Nullable ClientLevel clientLevel, @Nullable LivingEntity livingEntity, int seed) {
		stackRenderState.appendModelIdentityElement(this);
		final int[] tints = new int[this.tints.size()];
		for (int j = 0; j < tints.length; j++) {
			tints[j] = this.tints.get(j).calculate(stack, clientLevel, livingEntity);
			stackRenderState.appendModelIdentityElement(tints[j]);
		}

		ItemStackRenderState.LayerRenderState renderLayer = stackRenderState.newLayer();
		if (stack.hasFoil()) {
			renderLayer.setFoilType(ItemStackRenderState.FoilType.STANDARD);
			stackRenderState.appendModelIdentityElement(ItemStackRenderState.FoilType.STANDARD);
		}

		int[] tintLayers = renderLayer.prepareTintLayers(tints.length);
		System.arraycopy(tints, 0, tintLayers, 0, tints.length);

		setBackpackModelProperties(stack, stackRenderState);

		renderLayer.setExtents(extents);
		renderLayer.setUsesBlockLight(true);
		renderLayer.setParticleIcon(baseModel.particleIcon());
		renderLayer.setTransform(ITEM_TRANSFORMS.getTransform(displayContext));
		renderLayer.prepareQuadList().addAll(baseModel.getQuads());
		SpecialRenderer specialRenderer = new SpecialRenderer();
		specialRenderer.setModelRenderParameters(tintLayers, baseModel.getQuads());
		specialRenderer.displayItem = BackpackWrapper.fromStack(stack).getRenderInfo().getItemDisplayRenderInfo().getDisplayItem().map(displayItem -> {
			stackRenderState.appendModelIdentityElement(displayItem.getItem().getItem());
			stackRenderState.appendModelIdentityElement(displayItem.getItem().getComponents());
			stackRenderState.appendModelIdentityElement(displayItem.getRotation());
			return displayItem;
		}).orElse(null);

		renderLayer.setupSpecialModel(specialRenderer, specialRenderer.extractArgument(stack));
	}

	private void setBackpackModelProperties(ItemStack stack, ItemStackRenderState stackRenderState) {
		if (baseModel instanceof BackpackBlockModel.BlockStateModel backpackModel) {
			backpackModel.tankRight = false;
			backpackModel.tankLeft = false;
			backpackModel.battery = false;
			IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
			RenderInfo renderInfo = backpackWrapper.getRenderInfo();
			Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> tankRenderInfos = renderInfo.getTankRenderInfos();
			tankRenderInfos.forEach((pos, info) -> {
				if (pos == TankPosition.LEFT) {
					backpackModel.tankLeft = true;
					backpackModel.leftTankRenderInfo = info;
					stackRenderState.appendModelIdentityElement(TankPosition.LEFT);
					info.getFluid().ifPresent(fs -> {
						stackRenderState.appendModelIdentityElement(fs.getFluid());
						stackRenderState.appendModelIdentityElement(fs.getComponents());
							});
					stackRenderState.appendModelIdentityElement(info.getFillRatio());
				} else {
					backpackModel.tankRight = true;
					backpackModel.rightTankRenderInfo = info;
					stackRenderState.appendModelIdentityElement(TankPosition.RIGHT);
					info.getFluid().ifPresent(fs -> {
						stackRenderState.appendModelIdentityElement(fs.getFluid());
						stackRenderState.appendModelIdentityElement(fs.getComponents());
					});
					stackRenderState.appendModelIdentityElement(info.getFillRatio());
				}
			});

			renderInfo.getBatteryRenderInfo().ifPresent(batteryRenderInfo -> {
				backpackModel.battery = true;
				backpackModel.batteryRenderInfo = batteryRenderInfo;
				stackRenderState.appendModelIdentityElement("battery");
				stackRenderState.appendModelIdentityElement(batteryRenderInfo.getChargeRatio());
			});
		}
	}

	public record Unbaked(ResourceLocation base, List<ItemTintSource> tints) implements ItemModel.Unbaked {
		public static final MapCodec<Unbaked> MAP_CODEC = RecordCodecBuilder.mapCodec(builder -> builder.group(
				ResourceLocation.CODEC.fieldOf("base").forGetter(Unbaked::base),
				ItemTintSources.CODEC.listOf().optionalFieldOf("tints", List.of()).forGetter(Unbaked::tints)
		).apply(builder, Unbaked::new));

		@Override
		public MapCodec<? extends ItemModel.Unbaked> type() {
			return MAP_CODEC;
		}

		@Override
		public ItemModel bake(BakingContext context) {
			ResolvedModel resolved = context.blockModelBaker().getModel(base);
			if (resolved.wrapped() instanceof BackpackBlockModel baseModel) {
				return new BackpackItemModel(baseModel.bakeBlockStateModel(context.blockModelBaker(), resolved, BlockModelRotation.X0_Y0), tints);
			}

			throw new IllegalStateException("Expected a BackpackBlockModel, but got " + resolved.getClass().getName());
		}

		@Override
		public void resolveDependencies(Resolver resolver) {
			resolver.markDependency(base);
		}
	}

	public static class SpecialRenderer implements NoDataSpecialModelRenderer {
		private final Minecraft minecraft = Minecraft.getInstance();
		@Nullable
		public RenderInfo.DisplayItem displayItem = null;
		private int[] tintLayers;
		private List<BakedQuad> baseModel;

		@Override
		public void render(ItemDisplayContext displayContext, PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int packedOverlay, boolean hasFoil) {
			ItemRenderer.renderItem(
					displayContext,
					poseStack,
					buffer,
					combinedLight,
					packedOverlay,
					tintLayers,
					baseModel,
					Sheets.translucentItemSheet(),
					hasFoil ? ItemStackRenderState.FoilType.STANDARD : ItemStackRenderState.FoilType.NONE
			);
			if (displayItem != null) {
				poseStack.translate(0.5, 0.6, 0.25);
				poseStack.scale(0.5f, 0.5f, 0.5f);
				poseStack.mulPose(Axis.ZP.rotationDegrees(displayItem.getRotation()));
				ItemRenderer itemRenderer = minecraft.getItemRenderer();
				itemRenderer.renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, combinedLight, packedOverlay, poseStack, buffer, minecraft.level, 0);
			}
		}

		public void setModelRenderParameters(int[] tintLayers, List<BakedQuad> baseModel) {
			this.tintLayers = tintLayers;
			this.baseModel = baseModel;
		}

		@Override
		public void getExtents(Set<Vector3f> set) {
			//noop - not used in backpack item model as they are provided directly by itself
		}
	}
}
