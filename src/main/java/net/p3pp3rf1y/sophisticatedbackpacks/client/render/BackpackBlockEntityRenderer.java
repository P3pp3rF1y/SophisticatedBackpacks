package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.client.renderer.blockentity.state.BlockEntityRenderState;
import net.minecraft.client.renderer.feature.ModelFeatureRenderer;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.Direction;
import net.minecraft.resources.Identifier;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import net.neoforged.neoforge.client.extensions.common.IClientFluidTypeExtensions;
import net.neoforged.neoforge.client.textures.FluidSpriteCache;
import net.neoforged.neoforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class BackpackBlockEntityRenderer implements BlockEntityRenderer<BackpackBlockEntity, BackpackBlockEntityRenderer.BackpackRenderState> {
	private final ItemModelResolver itemModelResolver;

	public BackpackBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
		this.itemModelResolver = context.itemModelResolver();
	}

	private void renderItemDisplay(SubmitNodeCollector submitNodeCollector, BackpackRenderState renderState, PoseStack poseStack) {
		if (renderState.displayItem.isEmpty()) {
			return;
		}
		poseStack.pushPose();
		poseStack.translate(0, 0.6, 0.25);
		poseStack.scale(0.5f, 0.5f, 0.5f);
		poseStack.mulPose(Axis.XN.rotationDegrees(180));
		poseStack.mulPose(Axis.ZP.rotationDegrees(180f + renderState.displayItemRotation));
		renderState.displayItem.submit(poseStack, submitNodeCollector, renderState.lightCoords, OverlayTexture.NO_OVERLAY, 0);
		poseStack.popPose();
	}

	@Override
	public BackpackRenderState createRenderState() {
		return new BackpackRenderState();
	}

	@Override
	public void extractRenderState(BackpackBlockEntity blockEntity, BackpackRenderState renderState, float partialTick, Vec3 cameraPos, ModelFeatureRenderer.@Nullable CrumblingOverlay crumblingOverlay) {
		BlockEntityRenderer.super.extractRenderState(blockEntity, renderState, partialTick, cameraPos, crumblingOverlay);

		BlockState state = blockEntity.getBlockState();
		renderState.facing = state.getValue(BackpackBlock.FACING);

		IBackpackWrapper backpackWrapper = blockEntity.getBackpackWrapper();
		RenderDataHandler renderDataHandler = backpackWrapper.getRenderDataHandler();

		RenderData.DisplayData displayData = renderDataHandler.getDisplayData();
		if (!displayData.displayItems().isEmpty()) {
			RenderData.DisplayItemData displayItem = displayData.displayItems().getFirst();
			itemModelResolver.updateForTopItem(renderState.displayItem, displayItem.item(), ItemDisplayContext.FIXED, blockEntity.getLevel(), null, 0);
			renderState.displayItemRotation = displayItem.rotation();
		} else {
			renderState.displayItem = new ItemStackRenderState();
			renderState.displayItemRotation = 0;
		}

		renderState.model = BackpackModelManager.getBackpackModel(backpackWrapper.getBackpack().getItem());

		renderState.tanks = renderDataHandler.getTankRenderData().entrySet().stream()
				.filter(entry -> entry.getValue().getFluid().isPresent())
				.collect(Collectors.toMap(
						Map.Entry::getKey,
						entry -> {
							FluidStack fluidStack = entry.getValue().getFluid().get();
							IClientFluidTypeExtensions renderProperties = IClientFluidTypeExtensions.of(fluidStack.getFluid());
							Identifier texture = renderProperties.getStillTexture(fluidStack);
							TextureAtlasSprite still = FluidSpriteCache.getSprite(texture);
							return new BackpackRenderState.TankState(still, renderProperties.getTintColor(fluidStack), entry.getValue().fillRatio());
						}
				));
		renderState.batteryChargeRatio = renderDataHandler.getBatteryRenderData().map(RenderData.BatteryRenderData::chargeRatio).orElse(0f);

		renderState.showLeftTank = state.getValue(BackpackBlock.LEFT_TANK);
		renderState.showRightTank = state.getValue(BackpackBlock.RIGHT_TANK);
		renderState.showBattery = state.getValue(BackpackBlock.BATTERY);
	}

	@Override
	public void submit(BackpackRenderState renderState, PoseStack poseStack, SubmitNodeCollector submitNodeCollector, CameraRenderState cameraRenderState) {
		if (renderState.model == null) {
			return;
		}
		poseStack.pushPose();
		poseStack.translate(0.5, 0, 0.5);
		poseStack.mulPose(Axis.YN.rotationDegrees(renderState.facing.toYRot()));
		poseStack.pushPose();
		poseStack.scale(6 / 10f, 6 / 10f, 6 / 10f);
		poseStack.mulPose(Axis.ZP.rotationDegrees(180));
		poseStack.translate(0, -2.5, 0);
		if (renderState.showLeftTank) {
			submitTankFluid(renderState, poseStack, submitNodeCollector, TankPosition.LEFT, renderState.model);
		}
		if (renderState.showRightTank) {
			submitTankFluid(renderState, poseStack, submitNodeCollector, TankPosition.RIGHT, renderState.model);
		}
		poseStack.popPose();
		if (renderState.showBattery && renderState.batteryChargeRatio > 0.1f) {
			poseStack.pushPose();
			poseStack.mulPose(Axis.XN.rotationDegrees(180));
			poseStack.translate(0, -1.5, 0);
			renderState.model.submitBatteryCharge(submitNodeCollector, poseStack, renderState.lightCoords, renderState.batteryChargeRatio);
			poseStack.popPose();
		}
		renderItemDisplay(submitNodeCollector, renderState, poseStack);
		poseStack.popPose();
	}

	private static void submitTankFluid(BackpackRenderState renderState, PoseStack poseStack, SubmitNodeCollector submitNodeCollector, TankPosition tankPosition, IBackpackModel model) {
		if (renderState.tanks.get(tankPosition) != null) {
			poseStack.pushPose();
			poseStack.translate(tankPosition == TankPosition.LEFT ? 1.45 : -1.45, 0, 0);
			BackpackRenderState.TankState tankState = renderState.tanks.get(tankPosition);
			model.submitFluid(submitNodeCollector, poseStack, tankState.sprite(), tankState.fill(), tankState.color(), tankPosition == TankPosition.LEFT, renderState.lightCoords);
			poseStack.popPose();
		}
	}

	public static class BackpackRenderState extends BlockEntityRenderState {
		public ItemStackRenderState displayItem = new ItemStackRenderState();
		public Direction facing = Direction.NORTH;
		public int displayItemRotation = 0;
		public Map<TankPosition, TankState> tanks = new HashMap<>();
		public float batteryChargeRatio = 0f;
		public boolean showLeftTank = false;
		public boolean showRightTank = false;
		public boolean showBattery = false;
		@Nullable
		public IBackpackModel model = null;

		public record TankState(TextureAtlasSprite sprite, int color, float fill) {
		}
	}
}
