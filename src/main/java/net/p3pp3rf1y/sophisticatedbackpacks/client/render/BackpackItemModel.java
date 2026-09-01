package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

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
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.item.ItemModel;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.client.renderer.special.NoDataSpecialModelRenderer;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;

import javax.annotation.Nullable;

import java.util.List;
import java.util.Map;

public class BackpackItemModel implements ItemModel {
	private final SpecialRenderer specialRenderer = new SpecialRenderer();
	private final BakedModel baseModel;
	private final List<ItemTintSource> tints;

	public BackpackItemModel(BakedModel baseModel, List<ItemTintSource> tints) {
		this.baseModel = baseModel;
		this.tints = tints;
		if (baseModel instanceof BackpackBlockModel.Baked backpackModel) {
			specialRenderer.displayItemQuad = backpackModel.getDisplayItemQuad();
		}
	}

	@Override
	public void update(ItemStackRenderState stackRenderState, ItemStack stack, ItemModelResolver itemModelResolver, ItemDisplayContext displayContext,
			@Nullable ClientLevel clientLevel, @Nullable LivingEntity livingEntity, int seed) {
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

		RenderType renderType = baseModel.getRenderType(stack);
		renderLayer.setupBlockModel(baseModel, renderType);

		specialRenderer.setModelRenderParameters(tintLayers, baseModel, renderType);
		specialRenderer.displayItem = BackpackRenderInfo.fromPhysicalStack(stack).getItemDisplayRenderInfo().getDisplayItem().orElse(null);

		renderLayer.setupSpecialModel(specialRenderer, specialRenderer.extractArgument(stack), baseModel);
	}

	private void setBackpackModelProperties(ItemStack stack) {
		if (baseModel instanceof BackpackBlockModel.Baked backpackModel) {
			backpackModel.tankRight = false;
			backpackModel.tankLeft = false;
			backpackModel.rightTankRenderInfo = null;
			backpackModel.leftTankRenderInfo = null;
			backpackModel.battery = false;
			backpackModel.batteryRenderInfo = null;
			RenderInfo renderInfo = BackpackRenderInfo.fromPhysicalStack(stack);
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

	public BakedModel getBaseModel() {
		return baseModel;
	}

	public record Unbaked(ResourceLocation base, List<ItemTintSource> tints) implements ItemModel.Unbaked {
		public static final MapCodec<Unbaked> MAP_CODEC = RecordCodecBuilder
				.mapCodec(
						builder -> builder
								.group(ResourceLocation.CODEC.fieldOf("base").forGetter(Unbaked::base),
										ItemTintSources.CODEC.listOf().optionalFieldOf("tints", List.of()).forGetter(Unbaked::tints))
								.apply(builder, Unbaked::new));

		@Override
		public MapCodec<? extends ItemModel.Unbaked> type() {
			return MAP_CODEC;
		}

		@Override
		public ItemModel bake(BakingContext bakingContext) {
			return new BackpackItemModel(bakingContext.bake(base), tints);
		}

		@Override
		public void resolveDependencies(Resolver resolver) {
			resolver.resolve(base);
		}
	}

	public static class SpecialRenderer implements NoDataSpecialModelRenderer {
		private final Minecraft minecraft = Minecraft.getInstance();
		@Nullable
		public RenderInfo.DisplayItem displayItem = null;
		@Nullable
		public BakedQuad displayItemQuad = null;
		private int[] tintLayers;
		private BakedModel baseModel;
		private RenderType renderType;

		@Override
		public void render(ItemDisplayContext displayContext, PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int packedOverlay,
				boolean hasFoil) {
			ItemRenderer.renderItem(displayContext, poseStack, buffer, combinedLight, packedOverlay, tintLayers, baseModel, renderType,
					hasFoil ? ItemStackRenderState.FoilType.STANDARD : ItemStackRenderState.FoilType.NONE);
			if (displayItem != null) {
				if (displayItemQuad == null) {
					return;
				}
				DisplayItemAnchor.fromQuad(displayItemQuad).applyTransform(poseStack, displayItem.getZOffset());
				poseStack.mulPose(Axis.ZP.rotationDegrees(displayItem.getRotation()));
				ItemRenderer itemRenderer = minecraft.getItemRenderer();
				itemRenderer.renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, combinedLight, packedOverlay, poseStack, buffer, minecraft.level, 0);
			}
		}

		public void setModelRenderParameters(int[] tintLayers, BakedModel baseModel, RenderType renderType) {
			this.tintLayers = tintLayers;
			this.baseModel = baseModel;
			this.renderType = renderType;
		}
	}
}
