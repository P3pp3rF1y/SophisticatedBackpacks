package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.core.Direction;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;

import javax.annotation.Nullable;

public class BackpackBlockEntityRenderer implements BlockEntityRenderer<BackpackBlockEntity> {
	@Override
	public void render(BackpackBlockEntity backpackBlockEntity, float partialTicks, PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int combinedOverlay) {
		BlockState state = backpackBlockEntity.getBlockState();
		Direction facing = state.getValue(BackpackBlock.FACING);
		boolean showLeftTank = state.getValue(BackpackBlock.LEFT_TANK);
		boolean showRightTank = state.getValue(BackpackBlock.RIGHT_TANK);
		boolean showBattery = state.getValue(BackpackBlock.BATTERY);
		IBackpackWrapper backpackWrapper = backpackBlockEntity.getBackpackWrapper();
		RenderInfo renderInfo = backpackWrapper.getRenderInfo();
		poseStack.pushPose();
		poseStack.translate(0.5, 0, 0.5);
		poseStack.mulPose(Axis.YN.rotationDegrees(facing.toYRot()));
		poseStack.pushPose();
		poseStack.scale(6 / 10f, 6 / 10f, 6 / 10f);
		poseStack.mulPose(Axis.ZP.rotationDegrees(180));
		poseStack.translate(0, -2.5, 0);
		IBackpackModel model = BackpackModelManager.getBackpackModel(backpackWrapper.getBackpack().getItem());
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
					poseStack.mulPose(Axis.XN.rotationDegrees(180));
					poseStack.translate(0, -1.5, 0);
					model.renderBatteryCharge(poseStack, buffer, combinedLight, batteryRenderInfo.getChargeRatio());
					poseStack.popPose();
				}
			});
		}
		renderItemDisplay(poseStack, buffer, combinedLight, combinedOverlay, renderInfo, backpackBlockEntity.getLevel());
		poseStack.popPose();
	}

	private void renderItemDisplay(PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int combinedOverlay, RenderInfo renderInfo, @Nullable Level level) {
		renderInfo.getItemDisplayRenderInfo().getDisplayItem().ifPresent(displayItem -> {
			poseStack.pushPose();
			poseStack.translate(0, 0.6, 0.25);
			poseStack.scale(0.5f, 0.5f, 0.5f);
			poseStack.mulPose(Axis.XN.rotationDegrees(180));
			poseStack.mulPose(Axis.ZP.rotationDegrees(180f + displayItem.getRotation()));
			Minecraft.getInstance().getItemRenderer().renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, combinedLight, combinedOverlay, poseStack, buffer, level, 0);
			poseStack.popPose();
		});
	}
}
