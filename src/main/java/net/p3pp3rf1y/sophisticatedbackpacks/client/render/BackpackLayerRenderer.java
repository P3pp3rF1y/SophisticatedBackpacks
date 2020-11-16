package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.vertex.IVertexBuilder;
import net.minecraft.client.entity.player.AbstractClientPlayerEntity;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.entity.IEntityRenderer;
import net.minecraft.client.renderer.entity.layers.LayerRenderer;
import net.minecraft.client.renderer.entity.model.PlayerModel;
import net.minecraft.client.renderer.model.ModelRenderer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.vector.Vector3f;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import java.util.Map;

@OnlyIn(Dist.CLIENT)
public class BackpackLayerRenderer extends LayerRenderer<AbstractClientPlayerEntity, PlayerModel<AbstractClientPlayerEntity>> {
	private static final ResourceLocation BACKPACK_TEXTURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/entity/backpack.png");
	private static final BackpackModel MODEL = new BackpackModel();
	private static final Map<Item, ModelRenderer> BACKPACK_CLIPS = ImmutableMap.of(
			ModItems.BACKPACK, MODEL.leatherClips,
			ModItems.IRON_BACKPACK, MODEL.ironClips,
			ModItems.GOLD_BACKPACK, MODEL.goldClips,
			ModItems.DIAMOND_BACKPACK, MODEL.diamondClips
	);

	public BackpackLayerRenderer(IEntityRenderer<AbstractClientPlayerEntity, PlayerModel<AbstractClientPlayerEntity>> entityRendererIn) {
		super(entityRendererIn);
	}

	@Override
	public void render(MatrixStack matrixStack, IRenderTypeBuffer buffer, int packedLight, AbstractClientPlayerEntity player, float limbSwing, float limbSwingAmount, float partialTicks, float ageInTicks, float netHeadYaw, float headPitch) {
		PlayerInventoryProvider.getBackpackFromRendered(player).ifPresent(backpackRenderInfo -> {
			matrixStack.push();
			if (player.isCrouching()) {
				matrixStack.translate(0D, 0.2D, 0D);
				matrixStack.rotate(Vector3f.XP.rotationDegrees(90F / (float) Math.PI));
			}

			matrixStack.rotate(Vector3f.YP.rotationDegrees(180));
			float zOffset = backpackRenderInfo.isArmorSlot() || player.inventory.armorInventory.get(EquipmentSlotType.CHEST.getIndex()).isEmpty() ? -0.2f : -0.25f;
			matrixStack.translate(0, -0.75f, zOffset);

			ItemStack backpack = backpackRenderInfo.getBackpack();
			BackpackWrapper wrapper = new BackpackWrapper(backpack);

			IVertexBuilder vertexBuilder = ItemRenderer.getBuffer(buffer, RenderType.getEntityCutoutNoCull(BACKPACK_TEXTURE), false, false);

			int color = wrapper.getClothColor();
			float red = (color >> 16 & 255) / 255.0F;
			float green = (color >> 8 & 255) / 255.0F;
			float blue = (color & 255) / 255.0F;
			MODEL.cloth.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, red, green, blue, 1);

			color = wrapper.getBorderColor();
			red = (color >> 16 & 255) / 255.0F;
			green = (color >> 8 & 255) / 255.0F;
			blue = (color & 255) / 255.0F;
			MODEL.border.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, red, green, blue, 1);

			BACKPACK_CLIPS.getOrDefault(backpack.getItem(), MODEL.leatherClips).render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, 1, 1, 1, 1);

			matrixStack.pop();
		});
	}
}