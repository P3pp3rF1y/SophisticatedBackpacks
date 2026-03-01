package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.geom.EntityModelSet;
import net.minecraft.client.renderer.BlockEntityWithoutLevelRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderDispatcher;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;

public class BackpackItemStackRenderer extends BlockEntityWithoutLevelRenderer {
	private final Minecraft minecraft = Minecraft.getInstance();

	public BackpackItemStackRenderer(BlockEntityRenderDispatcher blockEntityRenderDispatcher, EntityModelSet entityModelSet) {
		super(blockEntityRenderDispatcher, entityModelSet);
	}

	@Override
	public void renderByItem(ItemStack stack, ItemDisplayContext transformType, PoseStack poseStack, MultiBufferSource buffer, int combinedLight, int combinedOverlay) {
		ItemRenderer itemRenderer = minecraft.getItemRenderer();
		BakedModel model = itemRenderer.getModel(stack, null, minecraft.player, 0);
		model.getRenderPasses(stack, true).forEach(bakedModel -> bakedModel.getRenderTypes(stack, true).forEach(renderType -> {
			itemRenderer.renderModelLists(bakedModel, stack, combinedLight, combinedOverlay, poseStack, buffer.getBuffer(renderType));
		}));

		IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
		backpackWrapper.getRenderInfo().getItemDisplayRenderInfo().getDisplayItem().ifPresent(displayItem -> {
			if (model instanceof BackpackDynamicModel.BackpackBakedModel backpackModel) {
				BakedQuad anchorQuad = backpackModel.getDisplayItemQuad();
				if (anchorQuad == null) {
					return;
				}
				poseStack.pushPose();
				DisplayItemAnchor.fromQuad(anchorQuad).applyTransform(poseStack);
				poseStack.mulPose(Axis.ZP.rotationDegrees(displayItem.getRotation()));
				itemRenderer.renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, combinedLight, combinedOverlay, poseStack, buffer, minecraft.level, 0);
				poseStack.popPose();
			}
		});
	}
}
