package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.base.Suppliers;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.client.Minecraft;
import net.minecraft.client.color.item.ItemTintSource;
import net.minecraft.client.color.item.ItemTintSources;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.Sheets;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.TextureSlots;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.item.*;
import net.minecraft.client.renderer.special.NoDataSpecialModelRenderer;
import net.minecraft.client.resources.model.BlockModelRotation;
import net.minecraft.client.resources.model.ModelBaker;
import net.minecraft.client.resources.model.ResolvedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.client.RenderTypeGroup;
import net.neoforged.neoforge.client.model.NeoForgeModelProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;
import org.joml.Vector3f;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

public class BackpackItemModel implements ItemModel {
	private final SpecialRenderer specialRenderer = new SpecialRenderer();
	private final BackpackBlockModel.BlockStateModel baseModel;
	private final List<ItemTintSource> tints;
	private final Supplier<Vector3f[]> extents;
	private final ModelRenderProperties properties;

	public BackpackItemModel(BackpackBlockModel.BlockStateModel baseModel, ModelRenderProperties properties, List<ItemTintSource> tints) {
		this.baseModel = baseModel;
		this.tints = tints;
		extents = Suppliers.memoize(() -> BlockModelWrapper.computeExtents(baseModel.getQuads()));
		this.properties = properties;
		if (baseModel instanceof BackpackBlockModel.BlockStateModel backpackModel) {
			specialRenderer.displayItemQuad = backpackModel.getDisplayItemQuad();
		}
	}

	@Override
	public void update(ItemStackRenderState stackRenderState, ItemStack stack, ItemModelResolver itemModelResolver, ItemDisplayContext displayContext, @Nullable ClientLevel clientLevel, @Nullable LivingEntity livingEntity, int seed) {
		final int[] tints = new int[this.tints.size()];
		for (int j = 0; j < tints.length; j++) {
			tints[j] = this.tints.get(j).calculate(stack, clientLevel, livingEntity);
		}

		ItemStackRenderState.LayerRenderState renderLayer = stackRenderState.newLayer();
		if (stack.hasFoil()) {
			renderLayer.setFoilType(ItemStackRenderState.FoilType.STANDARD);
		}

		int[] tintLayers = renderLayer.prepareTintLayers(tints.length);
		System.arraycopy(tints, 0, tintLayers, 0, tints.length);

		setBackpackModelProperties(stack);

		renderLayer.setExtents(extents); //TODO are these even required when specialRenderer actually does the rendering?
		properties.applyToLayer(renderLayer, displayContext);
		renderLayer.setUsesBlockLight(true);
		List<BakedQuad> quads = baseModel.getQuads(displayContext);
		renderLayer.setParticleIcon(baseModel.particleIcon());
		renderLayer.prepareQuadList().addAll(quads);
		specialRenderer.setModelRenderParameters(tintLayers, quads);
		specialRenderer.displayItem = BackpackWrapper.fromStack(stack).getRenderInfo().getItemDisplayRenderInfo().getDisplayItem().orElse(null);

		renderLayer.setupSpecialModel(specialRenderer, specialRenderer.extractArgument(stack));
	}

	private void setBackpackModelProperties(ItemStack stack) {
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
				} else {
					backpackModel.tankRight = true;
					backpackModel.rightTankRenderInfo = info;
				}
			});
			renderInfo.getBatteryRenderInfo().ifPresent(batteryRenderInfo -> {
				backpackModel.battery = true;
				backpackModel.batteryRenderInfo = batteryRenderInfo;
			});
		}
	}

	public BackpackBlockModel.BlockStateModel getBaseModel() {
		return baseModel;
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
			if (resolved.wrapped() instanceof BackpackBlockModel base) {
				TextureSlots textureslots = resolved.getTopTextureSlots();
				ModelBaker modelbaker = context.blockModelBaker();
				ModelRenderProperties modelRenderProperties = ModelRenderProperties.fromResolvedModel(modelbaker, resolved, textureslots);
				return new BackpackItemModel(base.bakeBlockStateModel(context.blockModelBaker(), resolved, BlockModelRotation.X0_Y0), modelRenderProperties, tints);
			}

			ModelBaker modelbaker = context.blockModelBaker();
			ResolvedModel resolvedmodel = modelbaker.getModel(base);
			TextureSlots textureslots = resolvedmodel.getTopTextureSlots();
			List<BakedQuad> list = resolvedmodel.bakeTopGeometry(textureslots, modelbaker, BlockModelRotation.X0_Y0).getAll();
			ModelRenderProperties modelrenderproperties = ModelRenderProperties.fromResolvedModel(modelbaker, resolvedmodel, textureslots);
			RenderTypeGroup renderTypeGroup = resolvedmodel.getTopAdditionalProperties().getOptional(NeoForgeModelProperties.RENDER_TYPE);
			RenderType renderType = renderTypeGroup == null ? null : renderTypeGroup.entity();
			return new BlockModelWrapper(tints, list, modelrenderproperties, renderType);
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
		@Nullable
		public BakedQuad displayItemQuad = null;
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
				if (displayItemQuad == null) {
					return;
				}
				DisplayItemAnchor.fromQuad(displayItemQuad).applyTransform(poseStack);
				poseStack.mulPose(Axis.ZP.rotationDegrees(displayItem.getRotation()));
				poseStack.mulPose(Axis.ZP.rotationDegrees(displayItem.getRotation()));
				ItemRenderer itemRenderer = minecraft.getItemRenderer();
				itemRenderer.renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, combinedLight, packedOverlay, poseStack, buffer, minecraft.level, 0);
			}
		}

		public void setModelRenderParameters(int[] tintLayers, List<BakedQuad> baseModel) {
			this.tintLayers = tintLayers;
			this.baseModel = baseModel;
		}
	}
}
