package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackModelManager;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.IBackpackModel;
import top.theillusivec4.curios.api.SlotContext;
import top.theillusivec4.curios.api.client.ICurioRenderer;

import javax.annotation.Nonnull;

public class BackpackCurioRenderer implements ICurioRenderer {
	@Override
	public <S extends LivingEntityRenderState, M extends EntityModel<? super S>> void render(ItemStack stack, SlotContext slotContext, PoseStack poseStack, @Nonnull MultiBufferSource renderTypeBuffer, int packedLight, S renderState, RenderLayerParent<S, M> renderLayerParent, EntityRendererProvider.Context context, float yRotation, float xRotation) {
		if (!stack.isEmpty()) {
			poseStack.pushPose();
			IBackpackModel model = BackpackModelManager.getBackpackModel(stack.getItem());
			EquipmentSlot equipmentSlot = model.getRenderEquipmentSlot();
			BackpackLayerRenderer.renderBackpack(renderLayerParent.getModel(), poseStack, renderTypeBuffer, packedLight, stack, !slotContext.entity().getItemBySlot(equipmentSlot).isEmpty(), BackpackModelManager.getBackpackModel(stack.getItem()), renderState, slotContext.entity().getType(), slotContext.entity().isBaby());
			poseStack.popPose();
		}
	}
}
