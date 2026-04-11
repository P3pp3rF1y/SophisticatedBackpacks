package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.util.context.ContextKey;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import javax.annotation.Nullable;
import java.util.function.BiConsumer;

public class BackpackLayerRenderer<S extends LivingEntityRenderState, M extends EntityModel<S>> extends RenderLayer<S, M> {
	private static final ContextKey<ItemStack> BACKPACK_ITEM_STACK = new ContextKey<>(SophisticatedBackpacks.getRL("backpack_item_stack"));
	private static final ContextKey<Boolean> WEARS_ARMOR = new ContextKey<>(SophisticatedBackpacks.getRL("wears_armor"));
	public static final ContextKey<EntityType<?>> ENTITY_TYPE = new ContextKey<>(SophisticatedBackpacks.getRL("entity_type"));
	public static final BiConsumer<LivingEntity, LivingEntityRenderState> RENDER_STATE_MODIFIER = (livingEntity, entityRenderState) -> {
		if (livingEntity instanceof Player player) {
			PlayerInventoryProvider.get().getBackpackFromRendered(player, false).ifPresent(backpackRenderInfo -> {
				ItemStack backpack = backpackRenderInfo.getBackpack();
				entityRenderState.setRenderData(BACKPACK_ITEM_STACK, backpack);
				entityRenderState.setRenderData(WEARS_ARMOR, !backpackRenderInfo.isArmorSlot() && !player.getItemBySlot(EquipmentSlot.CHEST).isEmpty());
			});
		} else {
			ItemStack chestStack = livingEntity.getItemBySlot(EquipmentSlot.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				entityRenderState.setRenderData(BACKPACK_ITEM_STACK, livingEntity.getItemBySlot(EquipmentSlot.CHEST));
				entityRenderState.setRenderData(WEARS_ARMOR, false);
			}
		}
		entityRenderState.setRenderData(ENTITY_TYPE, livingEntity.getType());
	};

	private static ItemRenderer itemRenderer;

	public BackpackLayerRenderer(RenderLayerParent<S, M> entityRendererIn) {
		super(entityRendererIn);
		itemRenderer = Minecraft.getInstance().getItemRenderer();
	}

	@Override
	public void render(PoseStack poseStack, MultiBufferSource multiBufferSource, int packedLight, S entityRenderState, float netHeadYaw, float headPitch) {
		ItemStack backpack = entityRenderState.getRenderData(BACKPACK_ITEM_STACK);
		if (backpack == null) {
			return;
		}
		poseStack.pushPose();
		boolean wearsArmor = entityRenderState.getRenderData(WEARS_ARMOR);
		boolean isBaby = entityRenderState.isBaby;
		EntityType<?> entityType = entityRenderState.getRenderData(ENTITY_TYPE);
		renderBackpack(getParentModel(), poseStack, multiBufferSource, packedLight, backpack, wearsArmor, entityType, isBaby);
		poseStack.popPose();
	}

	public static <S extends LivingEntityRenderState, M extends EntityModel<? super S>> void renderBackpack(M parentModel, PoseStack poseStack, MultiBufferSource buffer, int packedLight, ItemStack backpack, boolean wearsArmor, @Nullable EntityType<?> entityType, boolean isBaby) {
		translateRotateAndScale(parentModel, entityType, isBaby, poseStack, wearsArmor);
		itemRenderer.renderStatic(backpack, BackpackBlockModel.WORN, packedLight, OverlayTexture.NO_OVERLAY, poseStack, buffer, null, 0);
	}

	private static <S extends EntityRenderState, M extends EntityModel<? super S>> void translateRotateAndScale(M parentModel, @Nullable EntityType<?> entityType, boolean isBaby, PoseStack poseStack, boolean wearsArmor) {
		if (parentModel instanceof HumanoidModel<?> humanoidModel) {
			humanoidModel.body.translateAndRotate(poseStack);
		}

		poseStack.mulPose(Axis.YP.rotationDegrees(180));
		poseStack.mulPose(Axis.ZP.rotationDegrees(180));
		float zOffset = wearsArmor ? -0.35f : -0.3f;
		float yOffset = -0.25f;

		poseStack.translate(0, yOffset, zOffset);

		if (entityType == EntityType.PLAYER) {
			return;
		}

	}
}
