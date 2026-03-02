package net.p3pp3rf1y.sophisticatedbackpacks.compat.accessories;

import com.mojang.blaze3d.vertex.PoseStack;
import io.wispforest.accessories.api.client.AccessoryRenderer;
import io.wispforest.accessories.api.slot.SlotReference;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;

public class BackpackAccessoryRenderer implements AccessoryRenderer {
	@Override
	public <M extends LivingEntity> void render(ItemStack stack, SlotReference reference, PoseStack poseStack, EntityModel<M> model, MultiBufferSource multiBufferSource, int light, float limbSwing, float limbSwingAmount, float partialTicks, float ageInTicks, float netHeadYaw, float headPitch) {
		if (!stack.isEmpty()) {
			poseStack.pushPose();
			if (model instanceof HumanoidModel<?> parentModel) {
				BackpackLayerRenderer.renderBackpack(parentModel, reference.entity(), poseStack, multiBufferSource, light, stack, !reference.entity().getItemBySlot(EquipmentSlot.CHEST).isEmpty());
				poseStack.popPose();
			}
		}
	}
}
