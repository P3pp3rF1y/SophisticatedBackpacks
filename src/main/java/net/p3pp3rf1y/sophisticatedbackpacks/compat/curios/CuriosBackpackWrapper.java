package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.entity.LivingEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;
import top.theillusivec4.curios.api.type.capability.ICurio;

public class CuriosBackpackWrapper implements ICurio {
	@Override
	public boolean canRender(String identifier, int index, LivingEntity livingEntity) {
		return true;
	}

	@Override
	public void render(String identifier, int index, MatrixStack matrixStack, IRenderTypeBuffer renderTypeBuffer, int light, LivingEntity livingEntity, float limbSwing, float limbSwingAmount, float partialTicks, float ageInTicks, float netHeadYaw, float headPitch) {
		ItemStack backpack = CuriosCompat.getFromCuriosSlotStackHandler(livingEntity, identifier, sh -> sh.getStacks().getStackInSlot(index), ItemStack.EMPTY);
		if (!backpack.isEmpty()) {
			matrixStack.pushPose();
			BackpackLayerRenderer.renderBackpack(livingEntity, matrixStack, renderTypeBuffer, light, backpack, !livingEntity.getItemBySlot(EquipmentSlotType.CHEST).isEmpty());
			matrixStack.popPose();
		}
	}
}
