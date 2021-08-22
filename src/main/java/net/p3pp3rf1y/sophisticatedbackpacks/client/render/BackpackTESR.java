package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.block.BlockState;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.tileentity.TileEntityRenderer;
import net.minecraft.client.renderer.tileentity.TileEntityRendererDispatcher;
import net.minecraft.util.Direction;
import net.minecraft.util.math.vector.Vector3f;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.TankPosition;

public class BackpackTESR extends TileEntityRenderer<BackpackTileEntity> {
	public BackpackTESR(TileEntityRendererDispatcher rendererDispatcherIn) {
		super(rendererDispatcherIn);
	}

	@Override
	public void render(BackpackTileEntity tileEntityIn, float partialTicks, MatrixStack matrixStack, IRenderTypeBuffer buffer, int combinedLight, int combinedOverlayIn) {
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
		if (showLeftTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.LEFT);
			if (tankRenderInfo != null) {
				tankRenderInfo.getFluid().ifPresent(fluid -> RenderHelper.renderFluid(matrixStack, buffer, combinedLight, fluid, tankRenderInfo.getFillRatio(), -12.2F, 2.5F, 0, -2F));
			}
		}
		if (showRightTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.RIGHT);
			if (tankRenderInfo != null) {
				tankRenderInfo.getFluid().ifPresent(fluid -> RenderHelper.renderFluid(matrixStack, buffer, combinedLight, fluid, tankRenderInfo.getFillRatio(), 8.7F, 2.5F, 0, -2F));
			}
		}
		matrixStack.popPose();
		if (showBattery) {
			renderInfo.getBatteryRenderInfo().ifPresent(batteryRenderInfo -> {
				if (batteryRenderInfo.getChargeRatio() > 0.1f) {
					matrixStack.pushPose();
					matrixStack.mulPose(Vector3f.XN.rotationDegrees(180));
					RenderHelper.renderBatteryCharge(matrixStack, buffer, combinedLight, batteryRenderInfo.getChargeRatio());
					matrixStack.popPose();
				}
			});
		}
		matrixStack.popPose();
	}
}
