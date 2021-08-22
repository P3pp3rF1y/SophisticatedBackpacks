package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.vertex.IVertexBuilder;
import net.minecraft.client.renderer.entity.model.AgeableModel;
import net.minecraft.client.renderer.model.ModelRenderer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.entity.LivingEntity;
import net.minecraft.item.Item;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.Collections;
import java.util.Map;

public class BackpackModel extends AgeableModel<LivingEntity> {
	private final Map<Item, ModelRenderer> clipsBody;
	private final Map<Item, ModelRenderer> clipsLeftPouches;
	private final Map<Item, ModelRenderer> clipsRightPouches;
	private final Map<Item, ModelRenderer> clipsFrontPouch;
	private final Map<Item, ModelRenderer> clipsBattery;

	public final ModelRenderer cloth;
	private final ModelRenderer rightPouchesBorder;
	private final ModelRenderer leftPouchesBorder;
	private final ModelRenderer frontPouchBorder;
	private final ModelRenderer frontPouch;
	private final ModelRenderer rightPouches;
	private final ModelRenderer leftPouches;
	public final ModelRenderer border;
	private final ModelRenderer fabricFront;
	private final ModelRenderer fabricRight;
	private final ModelRenderer fabricLeft;
	public final ModelRenderer fabric;
	private final ModelRenderer battery;
	private final ModelRenderer batteryBorder;
	private final ModelRenderer leftTank;
	private final ModelRenderer leftTankBorder;
	private final ModelRenderer rightTank;
	private final ModelRenderer rightTankBorder;

	public BackpackModel() {
		texWidth = 64;
		texHeight = 64;

		cloth = new ModelRenderer(this);
		cloth.setPos(0.0F, 24.0F, 0.0F);
		cloth.texOffs(0, 0).addBox(-3.5F, -13.25F, -3.25F, 7.0F, 4.0F, 6.0F, 0.0F, false);
		cloth.texOffs(0, 10).addBox(-5.0F, -13.0F, -3.0F, 10.0F, 13.0F, 6.0F, 0.0F, false);

		rightPouchesBorder = new ModelRenderer(this);
		rightPouchesBorder.setPos(0.0F, 24.0F, 0.0F);
		rightPouchesBorder.texOffs(44, 0).addBox(5.0F, -2.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);

		leftPouchesBorder = new ModelRenderer(this);
		leftPouchesBorder.setPos(0.0F, 24.0F, 0.0F);
		leftPouchesBorder.texOffs(44, 0).addBox(-7.0F, -2.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);

		frontPouchBorder = new ModelRenderer(this);
		frontPouchBorder.setPos(0.0F, 24.0F, 0.0F);
		frontPouchBorder.texOffs(44, 0).addBox(-4.0F, -2.0F, -5.0F, 8.0F, 1.0F, 2.0F, 0.0F, false);

		frontPouch = new ModelRenderer(this);
		frontPouch.setPos(0.0F, 24.0F, 0.0F);
		frontPouch.texOffs(25, 0).addBox(-4.0F, -1.0F, -5.0F, 8.0F, 1.0F, 2.0F, 0.0F, false);
		frontPouch.texOffs(13, 2).addBox(-4.0F, -4.0F, -5.0F, 8.0F, 2.0F, 2.0F, 0.0F, false);
		frontPouch.texOffs(13, 0).addBox(-4.0F, -6.0F, -5.0F, 8.0F, 1.0F, 2.0F, 0.0F, false);

		rightPouches = new ModelRenderer(this);
		rightPouches.setPos(0.0F, 24.0F, 0.0F);
		rightPouches.texOffs(32, 5).addBox(5.0F, -1.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);
		rightPouches.texOffs(32, 13).addBox(5.0F, -4.0F, -2.5F, 2.0F, 2.0F, 5.0F, 0.0F, false);
		rightPouches.texOffs(32, 11).addBox(5.0F, -6.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);
		rightPouches.texOffs(32, 22).addBox(5.0F, -9.0F, -2.5F, 1.0F, 2.0F, 5.0F, 0.0F, false);
		rightPouches.texOffs(32, 20).addBox(5.0F, -11.0F, -2.5F, 1.0F, 1.0F, 5.0F, 0.0F, false);

		leftPouches = new ModelRenderer(this);
		leftPouches.setPos(0.0F, 24.0F, 0.0F);
		leftPouches.texOffs(32, 5).addBox(-7.0F, -1.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);
		leftPouches.texOffs(32, 13).addBox(-7.0F, -4.0F, -2.5F, 2.0F, 2.0F, 5.0F, 0.0F, true);
		leftPouches.texOffs(32, 11).addBox(-7.0F, -6.0F, -2.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);
		leftPouches.texOffs(32, 22).addBox(-6.0F, -9.0F, -2.5F, 1.0F, 2.0F, 5.0F, 0.0F, true);
		leftPouches.texOffs(32, 20).addBox(-6.0F, -11.0F, -2.5F, 1.0F, 1.0F, 5.0F, 0.0F, true);

		border = new ModelRenderer(this);
		border.setPos(0.0F, 24.0F, 0.0F);
		border.texOffs(44, 7).addBox(-3.5F, -9.25F, -3.25F, 7.0F, 1.0F, 1.0F, 0.0F, false);
		border.texOffs(50, 20).addBox(3.5F, -13.25F, -3.25F, 1.0F, 5.0F, 6.0F, 0.0F, false);
		border.texOffs(50, 9).addBox(-4.5F, -13.25F, -3.25F, 1.0F, 5.0F, 6.0F, 0.0F, false);

		fabricFront = new ModelRenderer(this);
		fabricFront.setPos(-3.25F, 16.0F, -6.0F);
		fabricFront.texOffs(0, 55).addBox(-0.75F, 3.0F, 1.0F, 8.0F, 1.0F, 2.0F, 0.0F, true);

		fabricRight = new ModelRenderer(this);
		fabricRight.setPos(-3.25F, 16.0F, -6.0F);
		fabricRight.texOffs(32, 49).addBox(8.25F, -2.0F, 3.5F, 1.0F, 1.0F, 5.0F, 0.0F, true);
		fabricRight.texOffs(8, 45).addBox(8.25F, 3.0F, 3.5F, 2.0F, 1.0F, 5.0F, 0.0F, true);

		fabricLeft = new ModelRenderer(this);
		fabricLeft.setPos(-3.25F, 16.0F, -6.0F);
		fabricLeft.texOffs(32, 49).addBox(-2.75F, -2.0F, 3.5F, 1.0F, 1.0F, 5.0F, 0.0F, false);
		fabricLeft.texOffs(8, 45).addBox(-3.75F, 3.0F, 3.5F, 2.0F, 1.0F, 5.0F, 0.0F, false);

		fabric = new ModelRenderer(this);
		fabric.setPos(-3.25F, 16.0F, -6.0F);
		fabric.texOffs(54, 0).addBox(1.25F, -4.75F, 5.75F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		fabric.texOffs(58, 0).addBox(4.25F, -4.75F, 5.75F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		fabric.texOffs(44, 0).addBox(1.25F, -5.75F, 5.75F, 4.0F, 1.0F, 1.0F, 0.0F, true);
		fabric.texOffs(16, 44).addBox(0.0F, -5.5F, 2.5F, 1.0F, 4.0F, 7.0F, 0.0F, false);
		fabric.texOffs(0, 44).addBox(5.5F, -5.5F, 2.5F, 1.0F, 4.0F, 7.0F, 0.0F, false);

		batteryBorder = new ModelRenderer(this);
		batteryBorder.setPos(0.0F, 24.0F, 0.0F);
		batteryBorder.texOffs(28, 38).addBox(-4.25F, -5.25F, -6.25F, 1.0F, 1.0F, 4.0F, 0.0F, false);
		batteryBorder.texOffs(28, 43).addBox(-3.5F, -5.25F, -6.25F, 7.0F, 1.0F, 1.0F, 0.0F, false);
		batteryBorder.texOffs(33, 38).addBox(-4.25F, -1.25F, -6.25F, 1.0F, 1.0F, 4.0F, 0.0F, false);
		batteryBorder.texOffs(33, 38).addBox(3.25F, -5.25F, -6.25F, 1.0F, 1.0F, 4.0F, 0.0F, false);
		batteryBorder.texOffs(27, 45).addBox(-3.5F, -1.25F, -6.25F, 7.0F, 1.0F, 1.0F, 0.0F, false);
		batteryBorder.texOffs(39, 37).addBox(3.25F, -1.25F, -6.25F, 1.0F, 1.0F, 4.0F, 0.0F, false);

		battery = new ModelRenderer(this);
		battery.setPos(0.0F, 24.0F, 0.0F);
		battery.texOffs(28, 29).addBox(-4.0F, -6.0F, -6.0F, 8.0F, 6.0F, 3.0F, 0.0F, false);
		battery.texOffs(28, 53).addBox(-2.0F, -6.25F, -4.5F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		battery.texOffs(28, 53).addBox(-0.75F, -6.25F, -4.5F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		battery.texOffs(28, 53).addBox(-2.0F, -8.0F, -3.25F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		battery.texOffs(28, 53).addBox(-0.75F, -8.0F, -3.25F, 1.0F, 1.0F, 1.0F, 0.0F, false);
		battery.texOffs(0, 58).addBox(-2.0F, -7.4F, -4.5F, 1.0F, 2.0F, 1.0F, -0.2F, false);
		battery.texOffs(6, 58).addBox(-0.75F, -7.4F, -4.5F, 1.0F, 2.0F, 1.0F, -0.2F, false);
		battery.texOffs(0, 61).addBox(-2.0F, -8.0F, -4.5F, 1.0F, 1.0F, 2.0F, -0.2F, false);
		battery.texOffs(6, 61).addBox(-0.75F, -8.0F, -4.5F, 1.0F, 1.0F, 2.0F, -0.2F, false);

		leftTankBorder = new ModelRenderer(this);
		leftTankBorder.setPos(0.0F, 24.0F, 0.0F);
		leftTankBorder.texOffs(50, 43).addBox(-8.0F, -9.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, false);

		leftTank = new ModelRenderer(this);
		leftTank.setPos(0.0F, 24.0F, 0.0F);
		leftTank.texOffs(54, 27).addBox(-5.5F, -7.5F, -2.0F, 1.0F, 6.0F, 4.0F, 0.0F, false);
		leftTank.texOffs(50, 37).addBox(-8.0F, -1.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, false);
		leftTank.texOffs(50, 42).addBox(-8.0F, -8.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, false);
		leftTank.texOffs(50, 37).addBox(-8.0F, -10.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, false);
		leftTank.texOffs(52, 48).addBox(-7.5F, -11.5F, -1.5F, 3.0F, 1.0F, 3.0F, 0.0F, false);

		rightTankBorder = new ModelRenderer(this);
		rightTankBorder.setPos(0.0F, 24.0F, 0.0F);
		rightTankBorder.texOffs(50, 43).addBox(5.0F, -9.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, true);

		rightTank = new ModelRenderer(this);
		rightTank.setPos(0.0F, 24.0F, 0.0F);
		rightTank.texOffs(54, 27).addBox(4.5F, -7.5F, -2.0F, 1.0F, 6.0F, 4.0F, 0.0F, true);
		rightTank.texOffs(50, 37).addBox(5.0F, -1.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, true);
		rightTank.texOffs(50, 42).addBox(5.0F, -8.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, true);
		rightTank.texOffs(50, 37).addBox(5.0F, -10.5F, -2.0F, 3.0F, 1.0F, 4.0F, 0.0F, true);
		rightTank.texOffs(52, 48).addBox(4.5F, -11.5F, -1.5F, 3.0F, 1.0F, 3.0F, 0.0F, true);

		clipsBody = ImmutableMap.of(
				ModItems.BACKPACK.get(), getBodyClipsRenderer(29),
				ModItems.IRON_BACKPACK.get(), getBodyClipsRenderer(32),
				ModItems.GOLD_BACKPACK.get(), getBodyClipsRenderer(35),
				ModItems.DIAMOND_BACKPACK.get(), getBodyClipsRenderer(38),
				ModItems.NETHERITE_BACKPACK.get(), getBodyClipsRenderer(41)
		);
		clipsLeftPouches = ImmutableMap.of(
				ModItems.BACKPACK.get(), getLeftPouchesClipsRenderer(29),
				ModItems.IRON_BACKPACK.get(), getLeftPouchesClipsRenderer(32),
				ModItems.GOLD_BACKPACK.get(), getLeftPouchesClipsRenderer(35),
				ModItems.DIAMOND_BACKPACK.get(), getLeftPouchesClipsRenderer(38),
				ModItems.NETHERITE_BACKPACK.get(), getLeftPouchesClipsRenderer(41)
		);
		clipsRightPouches = ImmutableMap.of(
				ModItems.BACKPACK.get(), getRightPouchesClipsRenderer(29),
				ModItems.IRON_BACKPACK.get(), getRightPouchesClipsRenderer(32),
				ModItems.GOLD_BACKPACK.get(), getRightPouchesClipsRenderer(35),
				ModItems.DIAMOND_BACKPACK.get(), getRightPouchesClipsRenderer(38),
				ModItems.NETHERITE_BACKPACK.get(), getRightPouchesClipsRenderer(41)
		);
		clipsFrontPouch = ImmutableMap.of(
				ModItems.BACKPACK.get(), getFrontPouchClipsRenderer(29),
				ModItems.IRON_BACKPACK.get(), getFrontPouchClipsRenderer(32),
				ModItems.GOLD_BACKPACK.get(), getFrontPouchClipsRenderer(35),
				ModItems.DIAMOND_BACKPACK.get(), getFrontPouchClipsRenderer(38),
				ModItems.NETHERITE_BACKPACK.get(), getFrontPouchClipsRenderer(41)
		);

		clipsBattery = ImmutableMap.of(
				ModItems.BACKPACK.get(), getBatteryClipsRenderer(30),
				ModItems.IRON_BACKPACK.get(), getBatteryClipsRenderer(33),
				ModItems.GOLD_BACKPACK.get(), getBatteryClipsRenderer(36),
				ModItems.DIAMOND_BACKPACK.get(), getBatteryClipsRenderer(39),
				ModItems.NETHERITE_BACKPACK.get(), getBatteryClipsRenderer(42)
		);
	}

	public void render(MatrixStack matrixStack, int packedLight, IVertexBuilder vertexBuilder, int clothColor, int borderColor, Item backpackItem, boolean showLeftTank, boolean showRightTank, boolean showBattery) {
		float borderRed = (borderColor >> 16 & 255) / 255.0F;
		float borderGreen = (borderColor >> 8 & 255) / 255.0F;
		float borderBlue = (borderColor & 255) / 255.0F;
		float clothRed = (clothColor >> 16 & 255) / 255.0F;
		float clothGreen = (clothColor >> 8 & 255) / 255.0F;
		float clothBlue = (clothColor & 255) / 255.0F;

		if (showLeftTank) {
			leftTank.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			leftTankBorder.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		} else {
			fabricLeft.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			clipsLeftPouches.get(backpackItem).render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			leftPouches.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);
			leftPouchesBorder.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		}

		if (showRightTank) {
			rightTank.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			rightTankBorder.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		} else {
			fabricRight.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			clipsRightPouches.get(backpackItem).render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			rightPouches.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);
			rightPouchesBorder.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		}

		if (showBattery) {
			battery.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			batteryBorder.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
			clipsBattery.get(backpackItem).render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
		} else {
			fabricFront.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			clipsFrontPouch.get(backpackItem).render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			frontPouch.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);
			frontPouchBorder.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		}

		fabric.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
		clipsBody.get(backpackItem).render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);

		cloth.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);

		border.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
	}

	private ModelRenderer getBodyClipsRenderer(int yTextureOffset) {
		ModelRenderer temp = new ModelRenderer(this);
		temp.setPos(0.0F, 24.0F, 0.0F);
		temp.texOffs(22, yTextureOffset).addBox(-3.25F, -9.5F, -3.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.texOffs(25, yTextureOffset).addBox(2.25F, -9.5F, -3.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		return temp;
	}

	private ModelRenderer getLeftPouchesClipsRenderer(int yTextureOffset) {
		ModelRenderer temp = new ModelRenderer(this);
		temp.setPos(0.0F, 24.0F, 0.0F);
		temp.texOffs(18, yTextureOffset).addBox(-6.25F, -10.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.texOffs(6, yTextureOffset).addBox(-7.25F, -5.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		return temp;
	}

	private ModelRenderer getRightPouchesClipsRenderer(int yTextureOffset) {
		ModelRenderer temp = new ModelRenderer(this);
		temp.setPos(0.0F, 24.0F, 0.0F);
		temp.texOffs(6, yTextureOffset).addBox(6.25F, -5.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, true);
		temp.texOffs(18, yTextureOffset).addBox(5.25F, -10.0F, -0.5F, 1.0F, 2.0F, 1.0F, 0.0F, true);
		return temp;
	}

	private ModelRenderer getFrontPouchClipsRenderer(int yTextureOffset) {
		ModelRenderer temp = new ModelRenderer(this);
		temp.setPos(0.0F, 24.0F, 0.0F);
		temp.texOffs(0, yTextureOffset).addBox(2.0F, -5.0F, -5.25F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		temp.texOffs(3, yTextureOffset).addBox(-3.0F, -5.0F, -5.25F, 1.0F, 2.0F, 1.0F, 0.0F, false);
		return temp;
	}

	private ModelRenderer getBatteryClipsRenderer(int yTextureOffset) {
		ModelRenderer temp = new ModelRenderer(this);
		temp.setPos(0.0F, 24.0F, 0.0F);
		temp.texOffs(24, yTextureOffset).addBox(1.0F, -5.25F, -6.15F, 1.0F, 1.0F, 1.0F, 0.2F, false);
		temp.texOffs(21, yTextureOffset).addBox(1.0F, -1.25F, -6.15F, 1.0F, 1.0F, 1.0F, 0.2F, false);
		return temp;
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

	@Override
	public void setupAnim(LivingEntity entityIn, float limbSwing, float limbSwingAmount, float ageInTicks, float netHeadYaw, float headPitch) {
		//noop
	}
}