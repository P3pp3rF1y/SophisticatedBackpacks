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
		texWidth = 32;
		texHeight = 32;

		leftTankGlass = new ModelRenderer(this);
		leftTankGlass.setPos(0.0F, 24.0F, 0.0F);
		leftTankGlass.texOffs(18, 5).addBox(-15F, 3.5F, -2.5F, 4.0F, 10.0F, 0.0F, 0.0F, false);
		leftTankGlass.texOffs(0, 0).addBox(-15F, 3.5F, -2.5F, 0.0F, 10.0F, 5.0F, 0.0F, false);
		leftTankGlass.texOffs(10, 5).addBox(-15F, 3.5F, 2.5F, 4.0F, 10.0F, 0.0F, 0.0F, false);

		rightTankGlass = new ModelRenderer(this);
		rightTankGlass.setPos(0.0F, 24.0F, 0.0F);
		rightTankGlass.texOffs(18, 5).addBox(11F, 3.5F, -2.5F, 4.0F, 10.0F, 0.0F, 0.0F, true);
		rightTankGlass.texOffs(0, 0).addBox(15F, 3.5F, -2.5F, 0.0F, 10.0F, 5.0F, 0.0F, true);
		rightTankGlass.texOffs(10, 5).addBox(11F, 3.5F, 2.5F, 4.0F, 10.0F, 0.0F, 0.0F, true);
	}

	@Override
	public void setupAnim(LivingEntity entityIn, float limbSwing, float limbSwingAmount, float ageInTicks, float netHeadYaw, float headPitch) {
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
	public void renderToBuffer(MatrixStack matrixStack, IVertexBuilder buffer, int packedLight, int packedOverlay, float red, float green, float blue, float alpha) {
		//noop
	}

	@Override
	protected Iterable<ModelRenderer> headParts() {
		return Collections.emptyList();
	}

	@Override
	protected Iterable<ModelRenderer> bodyParts() {
		return Collections.emptyList();
	}
}