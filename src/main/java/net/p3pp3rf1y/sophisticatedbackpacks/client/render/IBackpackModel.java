package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.material.Fluid;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;

public interface IBackpackModel {
	void render(PoseStack poseStack, MultiBufferSource buffer, int packedLight, int clothColor, int borderColor, Item backpackItem, RenderInfo renderInfo);

	void renderBatteryCharge(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, float chargeRatio);

	void renderFluid(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, Fluid fluid, float fill, boolean left);

	EquipmentSlot getRenderEquipmentSlot();

	<L extends LivingEntity, M extends HumanoidModel<L>> void translateRotateAndScale(M parentModel, LivingEntity livingEntity, PoseStack matrixStack, boolean wearsArmor);
}
