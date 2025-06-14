package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.item.Item;
import net.neoforged.neoforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;

public interface IBackpackModel {
	<S extends EntityRenderState, M extends EntityModel<? super S>> void render(M parentModel, S entityRenderState, PoseStack poseStack, MultiBufferSource buffer, int packedLight, int clothColor, int borderColor, Item backpackItem, RenderInfo renderInfo);

	void renderBatteryCharge(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, float chargeRatio);

	void renderFluid(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, FluidStack fluid, float fill, boolean left);

	EquipmentSlot getRenderEquipmentSlot();

	<S extends EntityRenderState, M extends EntityModel<? super S>> void translateRotateAndScale(M parentModel, EntityType<?> entityType, boolean isBaby, PoseStack poseStack, boolean wearsArmor);
}
