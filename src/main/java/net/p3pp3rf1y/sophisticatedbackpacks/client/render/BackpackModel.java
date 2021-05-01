package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableList;
import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.vertex.IVertexBuilder;
import net.minecraft.client.renderer.entity.model.AgeableModel;
import net.minecraft.client.renderer.model.ModelRenderer;
import net.minecraft.entity.LivingEntity;

public class BackpackModel extends AgeableModel<LivingEntity> {
	final ModelRenderer cloth;
	final ModelRenderer border;
	final ModelRenderer fabric;
	final ModelRenderer leatherClips;
	final ModelRenderer ironClips;
	final ModelRenderer goldClips;
	final ModelRenderer diamondClips;
	final ModelRenderer netheriteClips;

	public BackpackModel() {
		textureWidth = 64;
		textureHeight = 64;

		cloth = new ModelRenderer(this);
		cloth.setRotationPoint(0.0F, 24.0F, 0.0F);
		cloth.setTextureOffset(25, 0).addBox(-4.0F, -1.0F, -5.0F, 8.0F, 1.0F, 2.0F, 0.0F, false);
		cloth.setTextureOffset(13, 2).addBox(-4.0F, -4.0F, -5.0F, 8.0F, 2.0F, 2.0F, 0.0F, false);
		cloth.setTextureOffset(13, 0).addBox(-4.0F, -6.0F, -5.0F, 8.0F, 1.0F, 2.0F, 0.0F, false);
		cloth.setTextureOffset(32, 5).addBox(-7.0F, -1.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);
		cloth.setTextureOffset(32, 5).addBox(5.0F, -1.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);
		cloth.setTextureOffset(32, 13).addBox(-7.0F, -4.0F, -2.5F, 2.0F, 2.0F, 5.0F, 0.0F, true);
		cloth.setTextureOffset(32, 13).addBox(5.0F, -4.0F, -2.5F, 2.0F, 2.0F, 5.0F, 0.0F, false);
		cloth.setTextureOffset(32, 11).addBox(-7.0F, -6.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);
		cloth.setTextureOffset(32, 11).addBox(5.0F, -6.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);
		cloth.setTextureOffset(32, 22).addBox(-6.0F, -9.0F, -2.5F, 1.0F, 2.0F, 5.0F, 0.0F, true);
		cloth.setTextureOffset(32, 22).addBox(5.0F, -9.0F, -2.5F, 1.0F, 2.0F, 5.0F, 0.0F, false);
		cloth.setTextureOffset(32, 20).addBox(-6.0F, -11.0F, -2.5F, 1.0F, 1.0F, 5.0F, 0.0F, true);
		cloth.setTextureOffset(32, 20).addBox(5.0F, -11.0F, -2.5F, 1.0F, 1.0F, 5.0F, 0.0F, false);
		cloth.setTextureOffset(0, 0).addBox(-3.5F, -13.25F, -3.25F, 7.0F, 4.0F, 6.0F, 0.0F, false);
		cloth.setTextureOffset(0, 10).addBox(-5.0F, -13.0F, -3.0F, 10.0F, 13.0F, 6.0F, 0.0F, false);

		border = new ModelRenderer(this);
		border.setRotationPoint(0.0F, 24.0F, 0.0F);
		border.setTextureOffset(44, 0).addBox(-4.0F, -2.0F, -5.0F, 8.0F, 1.0F, 2.0F, 0.0F, false);
		border.setTextureOffset(44, 0).addBox(-7.0F, -2.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);
		border.setTextureOffset(44, 0).addBox(5.0F, -2.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);
		border.setTextureOffset(44, 7).addBox(-3.5F, -9.25F, -3.25F, 7.0F, 1.0F, 1.0F, 0.0F, false);
		border.setTextureOffset(50, 20).addBox(3.5F, -13.25F, -3.25F, 1.0F, 5.0F, 6.0F, 0.0F, false);
		border.setTextureOffset(50, 9).addBox(-4.5F, -13.25F, -3.25F, 1.0F, 5.0F, 6.0F, 0.0F, false);

		fabric = new ModelRenderer(this);
		fabric.setRotationPoint(-3.25F, 16.0F, -6.0F);
		fabric.setTextureOffset(54, 0).addBox(1.25F, -4.75F, 5.75F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		fabric.setTextureOffset(58, 0).addBox(4.25F, -4.75F, 5.75F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		fabric.setTextureOffset(44, 0).addBox(1.25F, -5.75F, 5.75F, 4.0F, 1.0F, 1.0F, 0.0F, true);
		fabric.setTextureOffset(16, 44).addBox(0.0F, -5.5F, 2.5F, 1.0F, 4.0F, 7.0F, 0.0F, false);
		fabric.setTextureOffset(0, 44).addBox(5.5F, -5.5F, 2.5F, 1.0F, 4.0F, 7.0F, 0.0F, false);
		fabric.setTextureOffset(32, 49).addBox(-2.75F, -2.0F, 3.5F, 1.0F, 1.0F, 5.0F, 0.0F, false);
		fabric.setTextureOffset(32, 49).addBox(8.25F, -2.0F, 3.5F, 1.0F, 1.0F, 5.0F, 0.0F, true);
		fabric.setTextureOffset(8, 45).addBox(-3.75F, 3.0F, 3.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);
		fabric.setTextureOffset(8, 45).addBox(8.25F, 3.0F, 3.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);
		fabric.setTextureOffset(0, 55).addBox(-0.75F, 3.0F, 1.0F, 8.0F, 1.0F, 2.0F, 0.0F, true);

		leatherClips = getClipsRenderer(29);
		ironClips = getClipsRenderer(32);
		goldClips = getClipsRenderer(35);
		diamondClips = getClipsRenderer(38);
		netheriteClips = getClipsRenderer(41);
	}

	private ModelRenderer getClipsRenderer(int yTextureOffset) {
		ModelRenderer temp = new ModelRenderer(this);
		temp.setRotationPoint(0.0F, 24.0F, 0.0F);
		temp.setTextureOffset(0, yTextureOffset).addBox(2.0F, -5.0F, -5.25F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.setTextureOffset(3, yTextureOffset).addBox(-3.0F, -5.0F, -5.25F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.setTextureOffset(6, yTextureOffset).addBox(-7.25F, -5.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.setTextureOffset(18, yTextureOffset).addBox(-6.25F, -10.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.setTextureOffset(14, yTextureOffset).addBox(5.25F, -10.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.setTextureOffset(10, yTextureOffset).addBox(6.25F, -5.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.setTextureOffset(22, yTextureOffset).addBox(-3.25F, -9.5F, -3.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.setTextureOffset(25, yTextureOffset).addBox(2.25F, -9.5F, -3.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
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
		return ImmutableList.of(cloth, border, fabric, leatherClips);
	}

	@Override
	public void setRotationAngles(LivingEntity entityIn, float limbSwing, float limbSwingAmount, float ageInTicks, float netHeadYaw, float headPitch) {
		//noop
	}
}