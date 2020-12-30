package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableList;
import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.vertex.IVertexBuilder;
import net.minecraft.client.renderer.entity.model.AgeableModel;
import net.minecraft.client.renderer.model.ModelRenderer;
import net.minecraft.entity.LivingEntity;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BackpackModel extends AgeableModel<LivingEntity> {
	final ModelRenderer cloth;
	final ModelRenderer border;
	final ModelRenderer leatherClips;
	final ModelRenderer ironClips;
	final ModelRenderer goldClips;
	final ModelRenderer diamondClips;

	public BackpackModel() {
		textureWidth = 64;
		textureHeight = 64;

		cloth = new ModelRenderer(this);
		cloth.setRotationPoint(8.0F, 24.0F, -9.0F);
		cloth.setTextureOffset(0, 0).addBox(-13.0F, -11.0F, 6.0F, 10.0F, 11.0F, 4.0F, 0.0F, false);
		cloth.setTextureOffset(28, 0).addBox(-13.0F, -11.0F, 5.0F, 10.0F, 5.0F, 1.0F, 0.0F, false);
		cloth.setTextureOffset(50, 0).addBox(-11.0F, -6.0F, 5.0F, 6.0F, 1.0F, 1.0F, 0.0F, false);
		cloth.setTextureOffset(54, 2).addBox(-3.0F, -10.0F, 6.0F, 1.0F, 3.0F, 4.0F, 0.0F, false);
		cloth.setTextureOffset(54, 16).addBox(-3.0F, -4.0F, 7.0F, 2.0F, 3.0F, 3.0F, 0.0F, false);
		cloth.setTextureOffset(28, 12).addBox(-3.0F, -5.0F, 7.0F, 1.0F, 1.0F, 3.0F, 0.0F, false);
		cloth.setTextureOffset(36, 12).addBox(-14.0F, -5.0F, 7.0F, 1.0F, 1.0F, 3.0F, 0.0F, false);
		cloth.setTextureOffset(44, 12).addBox(-15.0F, -4.0F, 7.0F, 2.0F, 3.0F, 3.0F, 0.0F, false);
		cloth.setTextureOffset(54, 28).addBox(-14.0F, -10.0F, 6.0F, 1.0F, 3.0F, 4.0F, 0.0F, false);
		cloth.setTextureOffset(28, 24).addBox(-12.0F, -12.0F, 6.0F, 8.0F, 1.0F, 4.0F, 0.0F, false);

		border = new ModelRenderer(this);
		border.setRotationPoint(8.0F, 24.0F, -9.0F);
		border.setTextureOffset(0, 15).addBox(-13.0F, -11.0F, 6.0F, 10.0F, 11.0F, 4.0F, 0.0F, false);
		border.setTextureOffset(28, 6).addBox(-13.0F, -11.0F, 5.0F, 10.0F, 5.0F, 1.0F, 0.0F, false);
		border.setTextureOffset(50, 0).addBox(-11.0F, -6.0F, 5.0F, 6.0F, 1.0F, 1.0F, 0.0F, false);
		border.setTextureOffset(54, 9).addBox(-3.0F, -10.0F, 6.0F, 1.0F, 3.0F, 4.0F, 0.0F, false);
		border.setTextureOffset(54, 22).addBox(-3.0F, -4.0F, 7.0F, 2.0F, 3.0F, 3.0F, 0.0F, false);
		border.setTextureOffset(28, 16).addBox(-3.0F, -5.0F, 7.0F, 1.0F, 1.0F, 3.0F, 0.0F, false);
		border.setTextureOffset(36, 16).addBox(-14.0F, -5.0F, 7.0F, 1.0F, 1.0F, 3.0F, 0.0F, false);
		border.setTextureOffset(44, 18).addBox(-15.0F, -4.0F, 7.0F, 2.0F, 3.0F, 3.0F, 0.0F, false);
		border.setTextureOffset(54, 35).addBox(-14.0F, -10.0F, 6.0F, 1.0F, 3.0F, 4.0F, 0.0F, false);
		border.setTextureOffset(28, 29).addBox(-12.0F, -12.0F, 6.0F, 8.0F, 1.0F, 4.0F, 0.0F, false);
		border.setTextureOffset(0, 30).addBox(-6.0F, -13.0F, 6.0F, 1.0F, 1.0F, 4.0F, 0.0F, false);
		border.setTextureOffset(10, 30).addBox(-10.0F, -14.0F, 7.0F, 4.0F, 1.0F, 2.0F, 0.0F, false);
		border.setTextureOffset(0, 35).addBox(-11.0F, -13.0F, 6.0F, 1.0F, 1.0F, 4.0F, 0.0F, false);

		leatherClips = getClipsRenderer(0);
		ironClips = getClipsRenderer(6);
		goldClips = getClipsRenderer(12);
		diamondClips = getClipsRenderer(18);
	}

	private ModelRenderer getClipsRenderer(int xTextureOffset) {
		ModelRenderer temp = new ModelRenderer(this);
		temp.setRotationPoint(8.0F, 24.0F, -9.0F);
		temp.setTextureOffset(xTextureOffset, 40).addBox(-7.0F, -7.0F, 4.5F, 1.0F, 3.0F, 2.0F, 0.0F, false);
		temp.setTextureOffset(xTextureOffset, 45).addBox(-10.0F, -7.0F, 4.5F, 1.0F, 3.0F, 2.0F, 0.0F, false);
		return temp;
	}

	@Override
	public void render(MatrixStack matrixStack, IVertexBuilder buffer, int packedLight, int packedOverlay, float red, float green, float blue, float alpha) {
		//noop
	}

	@Override
	protected Iterable<ModelRenderer> getHeadParts() {
		return ImmutableList.of();
	}

	@Override
	protected Iterable<ModelRenderer> getBodyParts() {
		return ImmutableList.of(cloth, border, leatherClips);
	}

	@Override
	public void setRotationAngles(LivingEntity entityIn, float limbSwing, float limbSwingAmount, float ageInTicks, float netHeadYaw, float headPitch) {
		//noop
	}
}
