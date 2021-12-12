package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.vertex.IVertexBuilder;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.RenderTypeLookup;
import net.minecraft.client.renderer.model.IBakedModel;
import net.minecraft.client.renderer.model.ItemCameraTransforms;
import net.minecraft.client.renderer.tileentity.ItemStackTileEntityRenderer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.vector.Vector3f;
import net.minecraftforge.client.ForgeHooksClient;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;

public class BackpackISTER extends ItemStackTileEntityRenderer {
	private final Minecraft minecraft = Minecraft.getInstance();

	@Override
	public void renderByItem(ItemStack stack, ItemCameraTransforms.TransformType transformType, MatrixStack matrixStack, IRenderTypeBuffer buffer, int combinedLight, int combinedOverlay) {
		//ItemRenderer.render does transformations that would need to be transformed against in complicated way so rather pop the pose here and push the new one with the same transforms
		// applied in the correct order with the getModel
		matrixStack.popPose();
		matrixStack.pushPose();
		ItemRenderer itemRenderer = minecraft.getItemRenderer();
		IBakedModel model = itemRenderer.getModel(stack, null, minecraft.player);

		boolean leftHand = minecraft.player != null && minecraft.player.getOffhandItem() == stack;
		model = ForgeHooksClient.handleCameraTransforms(matrixStack, model, transformType, leftHand);
		matrixStack.translate(-0.5D, -0.5D, -0.5D);
		RenderType rendertype = RenderTypeLookup.getRenderType(stack, true);
		IVertexBuilder ivertexbuilder = ItemRenderer.getFoilBufferDirect(buffer, rendertype, true, stack.hasFoil());
		itemRenderer.renderModelLists(model, stack, combinedLight, combinedOverlay, matrixStack, ivertexbuilder);
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> {
			BackpackRenderInfo.ItemDisplayRenderInfo itemDisplayRenderInfo = backpackWrapper.getRenderInfo().getItemDisplayRenderInfo();
			ItemStack displayItem = itemDisplayRenderInfo.getItem();
			if (!displayItem.isEmpty()) {
				matrixStack.translate(0.5, 0.6, 0.25);
				matrixStack.scale(0.5f, 0.5f, 0.5f);
				matrixStack.mulPose(Vector3f.ZP.rotationDegrees(itemDisplayRenderInfo.getRotation()));
				itemRenderer.renderStatic(displayItem, ItemCameraTransforms.TransformType.FIXED, combinedLight, combinedOverlay, matrixStack, buffer);
			}
		});
	}
}
