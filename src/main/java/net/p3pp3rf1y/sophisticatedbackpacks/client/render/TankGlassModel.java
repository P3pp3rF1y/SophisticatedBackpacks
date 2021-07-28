package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.vertex.IVertexBuilder;
import net.minecraft.client.renderer.entity.model.AgeableModel;
import net.minecraft.client.renderer.model.ModelRenderer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.entity.LivingEntity;

import java.util.Collections;

public class TankGlassModel extends AgeableModel<LivingEntity> {
	public final ModelRenderer leftTankGlass;
	public final ModelRenderer rightTankGlass;

	public TankGlassModel() {
		textureWidth = 32;
		textureHeight = 32;

		leftTankGlass = new ModelRenderer(this);
		leftTankGlass.setRotationPoint(0.0F, 24.0F, 0.0F);
		leftTankGlass.setTextureOffset(18, 5).addBox(-15F, 3.5F, -2.5F, 4.0F, 10.0F, 0.0F, 0.0F, false);
		leftTankGlass.setTextureOffset(0, 0).addBox(-15F, 3.5F, -2.5F, 0.0F, 10.0F, 5.0F, 0.0F, false);
		leftTankGlass.setTextureOffset(10, 5).addBox(-15F, 3.5F, 2.5F, 4.0F, 10.0F, 0.0F, 0.0F, false);

		rightTankGlass = new ModelRenderer(this);
		rightTankGlass.setRotationPoint(0.0F, 24.0F, 0.0F);
		rightTankGlass.setTextureOffset(18, 5).addBox(11F, 3.5F, -2.5F, 4.0F, 10.0F, 0.0F, 0.0F, true);
		rightTankGlass.setTextureOffset(0, 0).addBox(15F, 3.5F, -2.5F, 0.0F, 10.0F, 5.0F, 0.0F, true);
		rightTankGlass.setTextureOffset(10, 5).addBox(11F, 3.5F, 2.5F, 4.0F, 10.0F, 0.0F, 0.0F, true);
	}

	@Override
	public void setRotationAngles(LivingEntity entityIn, float limbSwing, float limbSwingAmount, float ageInTicks, float netHeadYaw, float headPitch) {
		//noop
	}

	public void render(MatrixStack matrixStack, IVertexBuilder buffer, int packedLight, boolean showLeftTank, boolean showRightTank) {
		if (showLeftTank) {
			leftTankGlass.render(matrixStack, buffer, packedLight, OverlayTexture.NO_OVERLAY);
		}
		if (showRightTank) {
			rightTankGlass.render(matrixStack, buffer, packedLight, OverlayTexture.NO_OVERLAY);
		}
	}

	@Override
	public void render(MatrixStack matrixStack, IVertexBuilder buffer, int packedLight, int packedOverlay, float red, float green, float blue, float alpha) {
		//noop
	}

	@Override
	protected Iterable<ModelRenderer> getHeadParts() {
		return Collections.emptyList();
	}

	@Override
	protected Iterable<ModelRenderer> getBodyParts() {
		return Collections.emptyList();
	}
}