package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Vector3f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.ClientEventHandler.BACKPACK_LAYER;

public class BackpackBlockEntityRenderer implements BlockEntityRenderer<BackpackBlockEntity> {
	private final BackpackModel model;

	public BackpackBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
		model = new BackpackModel(context.bakeLayer(BACKPACK_LAYER));
	}

	@Override
	public void render(BackpackBlockEntity tileEntityIn, float partialTicks, PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int combinedOverlay) {
		BlockState state = tileEntityIn.getBlockState();
		Direction facing = state.getValue(BackpackBlock.FACING);
		boolean showLeftTank = state.getValue(BackpackBlock.LEFT_TANK);
		boolean showRightTank = state.getValue(BackpackBlock.RIGHT_TANK);
		boolean showBattery = state.getValue(BackpackBlock.BATTERY);
		RenderInfo renderInfo = tileEntityIn.getBackpackWrapper().getRenderInfo();
		poseStack.pushPose();
		poseStack.translate(0.5, 0, 0.5);
		poseStack.mulPose(Vector3f.YN.rotationDegrees(facing.toYRot()));
		poseStack.pushPose();
		poseStack.scale(6 / 10f, 6 / 10f, 6 / 10f);
		poseStack.mulPose(Vector3f.ZP.rotationDegrees(180));
		poseStack.translate(0, -2.5, 0);
		if (showLeftTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.LEFT);
			if (tankRenderInfo != null) {
				poseStack.pushPose();
				poseStack.translate(1.45, 0, 0);
				tankRenderInfo.getFluid().ifPresent(fluid -> model.renderFluid(poseStack, buffer, combinedLight, fluid, tankRenderInfo.getFillRatio(), true));
				poseStack.popPose();
			}
		}
		if (showRightTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.RIGHT);
			if (tankRenderInfo != null) {
				poseStack.pushPose();
				poseStack.translate(-1.45, 0, 0);
				tankRenderInfo.getFluid().ifPresent(fluid -> model.renderFluid(poseStack, buffer, combinedLight, fluid, tankRenderInfo.getFillRatio(), false));
				poseStack.popPose();
			}
		}
		poseStack.popPose();
		if (showBattery) {
			renderInfo.getBatteryRenderInfo().ifPresent(batteryRenderInfo -> {
				if (batteryRenderInfo.getChargeRatio() > 0.1f) {
					poseStack.pushPose();
					poseStack.mulPose(Vector3f.XN.rotationDegrees(180));
					poseStack.translate(0, -1.5, 0);
					model.renderBatteryCharge(poseStack, buffer, combinedLight, batteryRenderInfo.getChargeRatio());
					poseStack.popPose();
				}
			});
		}
		renderItemDisplay(poseStack, buffer, combinedLight, combinedOverlay, renderInfo);
		poseStack.popPose();
	}

	private void renderItemDisplay(PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int combinedOverlay, RenderInfo renderInfo) {
		BackpackRenderInfo.ItemDisplayRenderInfo itemDisplayRenderInfo = renderInfo.getItemDisplayRenderInfo();
		poseStack.pushPose();
		poseStack.translate(0, 0.6, 0.25);
		poseStack.scale(0.5f, 0.5f, 0.5f);
		poseStack.mulPose(Vector3f.XN.rotationDegrees(180));
		poseStack.mulPose(Vector3f.ZP.rotationDegrees(180f + itemDisplayRenderInfo.getRotation()));
		Minecraft.getInstance().getItemRenderer().renderStatic(itemDisplayRenderInfo.getItem(), ItemTransforms.TransformType.FIXED, combinedLight, combinedOverlay, poseStack, buffer, 0);
		poseStack.popPose();
	}
}
