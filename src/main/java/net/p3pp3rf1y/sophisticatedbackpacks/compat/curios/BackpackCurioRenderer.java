package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackModelManager;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.IBackpackModel;
import top.theillusivec4.curios.api.SlotContext;
import top.theillusivec4.curios.api.client.ICurioRenderer;

public class BackpackCurioRenderer implements ICurioRenderer {
	@Override
	public <T extends LivingEntity, M extends EntityModel<T>> void render(ItemStack stack, SlotContext slotContext, PoseStack matrixStack, RenderLayerParent<T, M> renderLayerParent, MultiBufferSource renderTypeBuffer, int light, float limbSwing, float limbSwingAmount, float partialTicks, float ageInTicks, float netHeadYaw, float headPitch) {
		if (!stack.isEmpty()) {
			matrixStack.pushPose();
			if (renderLayerParent.getModel() instanceof HumanoidModel<?> parentModel) {
				IBackpackModel model = BackpackModelManager.getBackpackModel(stack.getItem());
				EquipmentSlot equipmentSlot = model.getRenderEquipmentSlot();
				BackpackLayerRenderer.renderBackpack(parentModel, slotContext.getWearer(), matrixStack, renderTypeBuffer, light, stack, !slotContext.getWearer().getItemBySlot(equipmentSlot).isEmpty(), BackpackModelManager.getBackpackModel(stack.getItem()));
				matrixStack.popPose();
			}
		}
	}
}
