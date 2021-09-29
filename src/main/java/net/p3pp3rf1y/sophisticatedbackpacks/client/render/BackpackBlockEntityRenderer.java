package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Vector3f;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.TankPosition;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.ClientEventHandler.BACKPACK_LAYER;

public class BackpackBlockEntityRenderer implements BlockEntityRenderer<BackpackBlockEntity> {
	private final BackpackModel model;

	public BackpackBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
		model = new BackpackModel(context.bakeLayer(BACKPACK_LAYER));
	}

	@Override
	public void render(BackpackBlockEntity tileEntityIn, float partialTicks, PoseStack matrixStack, MultiBufferSource buffer, int combinedLight, int combinedOverlayIn) {
		BlockState state = tileEntityIn.getBlockState();
		Direction facing = state.getValue(BackpackBlock.FACING);
		boolean showLeftTank = state.getValue(BackpackBlock.LEFT_TANK);
		boolean showRightTank = state.getValue(BackpackBlock.RIGHT_TANK);
		boolean showBattery = state.getValue(BackpackBlock.BATTERY);
		BackpackRenderInfo renderInfo = tileEntityIn.getBackpackWrapper().getRenderInfo();
		matrixStack.pushPose();
		matrixStack.translate(0.5, 0, 0.5);
		matrixStack.mulPose(Vector3f.YN.rotationDegrees(facing.toYRot()));
		matrixStack.pushPose();
		matrixStack.scale(6 / 10f, 6 / 10f, 6 / 10f);
		matrixStack.translate(0, -1.69, 0);
		if (showLeftTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.LEFT);
			if (tankRenderInfo != null) {
				matrixStack.pushPose();
				matrixStack.translate(0.15, 0, 0);
				tankRenderInfo.getFluid().ifPresent(fluid -> model.renderFluid(matrixStack, buffer, combinedLight, fluid, tankRenderInfo.getFillRatio(), true));
				matrixStack.popPose();
			}
		}
		if (showRightTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.RIGHT);
			if (tankRenderInfo != null) {
				matrixStack.pushPose();
				matrixStack.translate(-0.15, 0, 0);
				tankRenderInfo.getFluid().ifPresent(fluid -> model.renderFluid(matrixStack, buffer, combinedLight, fluid, tankRenderInfo.getFillRatio(), false));
				matrixStack.popPose();
			}
		}
		matrixStack.popPose();
		if (showBattery) {
			renderInfo.getBatteryRenderInfo().ifPresent(batteryRenderInfo -> {
				if (batteryRenderInfo.getChargeRatio() > 0.1f) {
					matrixStack.pushPose();
					matrixStack.mulPose(Vector3f.XN.rotationDegrees(180));
					matrixStack.translate(0, -1.5, 0);
					model.renderBatteryCharge(matrixStack, buffer, combinedLight, batteryRenderInfo.getChargeRatio());
					matrixStack.popPose();
				}
			});
		}
		matrixStack.popPose();
	}
}
