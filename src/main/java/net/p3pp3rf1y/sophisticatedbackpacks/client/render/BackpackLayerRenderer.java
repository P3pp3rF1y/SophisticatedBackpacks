package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.util.context.ContextKey;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;

import java.util.function.BiConsumer;

public class BackpackLayerRenderer<S extends LivingEntityRenderState, M extends EntityModel<S>> extends RenderLayer<S, M> {
	public static final ContextKey<ItemStack> BACKPACK_ITEM_STACK = new ContextKey<>(SophisticatedBackpacks.getRL("backpack_item_stack"));
	private static final ContextKey<Boolean> WEARS_ARMOR = new ContextKey<>(SophisticatedBackpacks.getRL("wears_armor"));
	private static final ContextKey<EntityType<?>> ENTITY_TYPE = new ContextKey<>(SophisticatedBackpacks.getRL("entity_type"));
	public static final BiConsumer<LivingEntity, LivingEntityRenderState> RENDER_STATE_MODIFIER = (livingEntity, entityRenderState) -> {
		if (livingEntity instanceof Player player) {
			PlayerInventoryProvider.get().getBackpackFromRendered(player, false).ifPresent(backpackRenderInfo -> {
				ItemStack backpack = backpackRenderInfo.getBackpack();
				entityRenderState.setRenderData(BACKPACK_ITEM_STACK, backpack);
				IBackpackModel model = BackpackModelManager.getBackpackModel(backpack.getItem());
				EquipmentSlot equipmentSlot = model.getRenderEquipmentSlot();
				entityRenderState.setRenderData(WEARS_ARMOR, (equipmentSlot != EquipmentSlot.CHEST || !backpackRenderInfo.isArmorSlot()) && !player.getInventory().armor.get(equipmentSlot.getIndex()).isEmpty());
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

	public BackpackLayerRenderer(RenderLayerParent<S, M> entityRendererIn) {
		super(entityRendererIn);
		BackpackModelManager.initModels();
	}

	@Override
	public void render(PoseStack poseStack, MultiBufferSource multiBufferSource, int packedLight, S entityRenderState, float netHeadYaw, float headPitch) {
		ItemStack backpack = entityRenderState.getRenderData(BACKPACK_ITEM_STACK);
		if (backpack == null) {
			return;
		}
		IBackpackModel model = BackpackModelManager.getBackpackModel(backpack.getItem());
		poseStack.pushPose();
		boolean wearsArmor = entityRenderState.getRenderData(WEARS_ARMOR);
		boolean isBaby = entityRenderState.isBaby;
		EntityType<?> entityType = entityRenderState.getRenderData(ENTITY_TYPE);
		renderBackpack(getParentModel(), poseStack, multiBufferSource, packedLight, backpack, wearsArmor, model, entityRenderState, entityType, isBaby);
		poseStack.popPose();
	}

	public static <S extends LivingEntityRenderState, M extends EntityModel<? super S>> void renderBackpack(M parentModel, PoseStack matrixStack, MultiBufferSource buffer, int packedLight, ItemStack backpack, boolean wearsArmor, IBackpackModel model, S livingEntityRenderState, EntityType<?> entityType, boolean isBaby) {
		model.translateRotateAndScale(parentModel, entityType, isBaby, matrixStack, wearsArmor);

		IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
		int clothColor = wrapper.getMainColor();
		int borderColor = wrapper.getAccentColor();
		model.render(parentModel, livingEntityRenderState, matrixStack, buffer, packedLight, clothColor, borderColor, backpack.getItem(), wrapper.getRenderInfo());
		renderItemShown(matrixStack, buffer, packedLight, wrapper.getRenderInfo());
	}

	private static void renderItemShown(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, RenderInfo renderInfo) {
		renderInfo.getItemDisplayRenderInfo().getDisplayItem().ifPresent(displayItem -> {
			matrixStack.pushPose();
			matrixStack.translate(0, 0.9, -0.25);
			matrixStack.scale(0.5f, 0.5f, 0.5f);
			matrixStack.mulPose(Axis.ZP.rotationDegrees(180f + displayItem.getRotation()));
			Minecraft.getInstance().getItemRenderer().renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, packedLight, OverlayTexture.NO_OVERLAY, matrixStack, buffer, null, 0);
			matrixStack.popPose();
		});
	}
}
