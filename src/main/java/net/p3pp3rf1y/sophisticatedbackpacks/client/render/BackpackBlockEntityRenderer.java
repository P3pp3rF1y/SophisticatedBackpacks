package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;

import javax.annotation.Nullable;

public class BackpackBlockEntityRenderer implements BlockEntityRenderer<BackpackBlockEntity> {
	@Override
	public void render(BackpackBlockEntity backpackBlockEntity, float partialTicks, PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int combinedOverlay) {
		BlockState state = backpackBlockEntity.getBlockState();
		Direction facing = state.getValue(BackpackBlock.FACING);
		IBackpackWrapper backpackWrapper = backpackBlockEntity.getBackpackWrapper();
		RenderInfo renderInfo = backpackWrapper.getRenderInfo();
		poseStack.pushPose();
		poseStack.translate(0.5, 0, 0.5);
		poseStack.mulPose(Axis.YN.rotationDegrees(facing.toYRot()));
		poseStack.pushPose();
		poseStack.scale(6 / 10f, 6 / 10f, 6 / 10f);
		poseStack.mulPose(Axis.ZP.rotationDegrees(180));
		poseStack.translate(0, -2.5, 0);
		poseStack.popPose();
		renderItemDisplay(backpackBlockEntity.getBackpackWrapper().getBackpack(), poseStack, buffer, combinedLight, combinedOverlay, renderInfo, backpackBlockEntity.getLevel());
		poseStack.popPose();
	}

	private void renderItemDisplay(ItemStack backpack, PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int combinedOverlay, RenderInfo renderInfo, @Nullable Level level) {
		renderInfo.getItemDisplayRenderInfo().getDisplayItem().ifPresent(displayItem -> {
			Minecraft minecraft = Minecraft.getInstance();
			ItemRenderer itemRenderer = minecraft.getItemRenderer();
			BakedModel model = itemRenderer.getModel(backpack, null, minecraft.player, 0);
			if (model instanceof BackpackDynamicModel.BackpackBakedModel backpackModel) {
				BakedQuad anchorQuad = backpackModel.getDisplayItemQuad();
				if (anchorQuad == null) {
					return;
				}
				poseStack.pushPose();

				poseStack.translate(0.5, 0, 0.5);
				poseStack.mulPose(Axis.YP.rotationDegrees(180));

				DisplayItemAnchor.fromQuad(anchorQuad).applyTransform(poseStack);
				poseStack.mulPose(Axis.ZP.rotationDegrees(displayItem.getRotation()));
				itemRenderer.renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, combinedLight, combinedOverlay, poseStack, buffer, level, 0);

				poseStack.popPose();
			}
		});
	}
}
