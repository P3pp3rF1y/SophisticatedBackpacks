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
import net.minecraft.client.renderer.entity.model.BipedModel;
import net.minecraft.client.renderer.model.ModelRenderer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.math.vector.Vector3f;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import java.util.HashMap;
import java.util.Map;

public class BackpackLayerRenderer<T extends LivingEntity, M extends BipedModel<T>> extends LayerRenderer<T, M> {
	private static final float CHILD_Y_OFFSET = 0.3F;
	private static final float CHILD_Z_OFFSET = 0.1F;
	private static final float CHILD_SCALE = 0.55F;
	private static final ResourceLocation BACKPACK_TEXTURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/entity/backpack.png");

	private static final BackpackModel MODEL = new BackpackModel();
	private static final Map<Item, ModelRenderer> BACKPACK_CLIPS = ImmutableMap.of(
			ModItems.BACKPACK.get(), MODEL.leatherClips,
			ModItems.IRON_BACKPACK.get(), MODEL.ironClips,
			ModItems.GOLD_BACKPACK.get(), MODEL.goldClips,
			ModItems.DIAMOND_BACKPACK.get(), MODEL.diamondClips,
			ModItems.NETHERITE_BACKPACK.get(), MODEL.netheriteClips
	);

	private static final Map<EntityType<?>, Vector3d> entityTranslations;

	static {
		entityTranslations = new HashMap<>();
		entityTranslations.put(EntityType.ENDERMAN, new Vector3d(0, -0.8, 0));
	}

	public BackpackLayerRenderer(IEntityRenderer<T, M> entityRendererIn) {
		super(entityRendererIn);
	}

	@Override
	public void render(MatrixStack matrixStack, IRenderTypeBuffer buffer, int packedLight, T entity, float limbSwing, float limbSwingAmount, float partialTicks, float ageInTicks, float netHeadYaw, float headPitch) {
		if (entity instanceof AbstractClientPlayerEntity) {
			AbstractClientPlayerEntity player = (AbstractClientPlayerEntity) entity;
			PlayerInventoryProvider.getBackpackFromRendered(player).ifPresent(backpackRenderInfo -> {
				matrixStack.push();
				boolean wearsArmor = !backpackRenderInfo.isArmorSlot() && !player.inventory.armorInventory.get(EquipmentSlotType.CHEST.getIndex()).isEmpty();
				ItemStack backpack = backpackRenderInfo.getBackpack();
				renderBackpack(player, matrixStack, buffer, packedLight, backpack, wearsArmor);
				matrixStack.pop();
			});
		} else {
			ItemStack chestStack = entity.getItemStackFromSlot(EquipmentSlotType.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				renderBackpack(entity, matrixStack, buffer, packedLight, chestStack, false);
			}
		}
	}

	public static void renderBackpack(LivingEntity livingEntity, MatrixStack matrixStack, IRenderTypeBuffer buffer, int packedLight, ItemStack backpack, boolean wearsArmor) {
		if (livingEntity.isCrouching()) {
			matrixStack.translate(0D, 0.2D, 0D);
			matrixStack.rotate(Vector3f.XP.rotationDegrees(90F / (float) Math.PI));
		}

		matrixStack.rotate(Vector3f.YP.rotationDegrees(180));
		float zOffset = wearsArmor ? -0.35f : -0.3f;
		float yOffset = -0.75f;

		if (livingEntity.isChild()) {
			zOffset += CHILD_Z_OFFSET;
			yOffset = CHILD_Y_OFFSET;
		}

		matrixStack.translate(0, yOffset, zOffset);

		if (livingEntity.isChild()) {
			matrixStack.scale(CHILD_SCALE, CHILD_SCALE, CHILD_SCALE);
		}

		if (entityTranslations.containsKey(livingEntity.getType())) {
			Vector3d translVector = entityTranslations.get(livingEntity.getType());
			matrixStack.translate(translVector.getX(), translVector.getY(), translVector.getZ());
		}

		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
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

			MODEL.fabric.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, 1, 1, 1, 1);

			BACKPACK_CLIPS.getOrDefault(backpack.getItem(), MODEL.leatherClips).render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, 1, 1, 1, 1);

		});
	}
}