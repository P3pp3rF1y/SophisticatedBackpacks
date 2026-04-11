package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.client.renderer.item.ItemStackRenderState;
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
import org.jspecify.annotations.Nullable;

import java.util.function.BiConsumer;

public class BackpackLayerRenderer<S extends LivingEntityRenderState, M extends EntityModel<S>> extends RenderLayer<S, M> {
	private static final ContextKey<ItemStackRenderState> BACKPACK = new ContextKey<>(SophisticatedBackpacks.getIdentifier("backpack"));
	private static final ContextKey<Boolean> WEARS_ARMOR = new ContextKey<>(SophisticatedBackpacks.getIdentifier("wears_armor"));
	private static final ContextKey<EntityType<?>> ENTITY_TYPE = new ContextKey<>(SophisticatedBackpacks.getIdentifier("entity_type"));
	public static final BiConsumer<LivingEntity, LivingEntityRenderState> RENDER_STATE_MODIFIER = (livingEntity, entityRenderState) -> {
		if (livingEntity instanceof Player player) {
			PlayerInventoryProvider.get().getBackpackFromRendered(player, false).ifPresent(backpackRenderInfo ->
					addBackpackRenderState(entityRenderState, player, backpackRenderInfo));
		} else {
			ItemStack chestStack = livingEntity.getItemBySlot(EquipmentSlot.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				addBackpackRenderState(entityRenderState, livingEntity, chestStack, false);
			}
		}
		entityRenderState.setRenderData(ENTITY_TYPE, livingEntity.getType());
	};

	public static void addBackpackRenderState(LivingEntityRenderState entityRenderState, LivingEntity livingEntity, PlayerInventoryProvider.RenderInfo backpackRenderInfo) {
		addBackpackRenderState(entityRenderState, livingEntity, backpackRenderInfo.getBackpack(), !backpackRenderInfo.isArmorSlot() && !livingEntity.getItemBySlot(EquipmentSlot.CHEST).isEmpty());
	}

	private static void addBackpackRenderState(LivingEntityRenderState entityRenderState, LivingEntity livingEntity, ItemStack backpack, boolean wearsArmor) {
		entityRenderState.setRenderData(WEARS_ARMOR, wearsArmor);
		Minecraft mc = Minecraft.getInstance();
		ItemModelResolver itemModelResolver = mc.getItemModelResolver();
		ItemStackRenderState backpackRenderState = new ItemStackRenderState();
		itemModelResolver.updateForTopItem(backpackRenderState, backpack, BackpackBlockModel.WORN, mc.level, null, 0);
		entityRenderState.setRenderData(BACKPACK, backpackRenderState);
	}

	public BackpackLayerRenderer(RenderLayerParent<S, M> entityRendererIn) {
		super(entityRendererIn);
	}

	@Override
	public void submit(PoseStack poseStack, SubmitNodeCollector submitNodeCollector, int packedLight, S entityRenderState, float netHeadYaw, float headPitch) {
		poseStack.pushPose();
		submitBackpack(getParentModel(), entityRenderState, poseStack, submitNodeCollector, packedLight);
		poseStack.popPose();
	}

	public static <S extends LivingEntityRenderState, M extends EntityModel<? super S>> void submitBackpack(M parentModel, S entityRenderState, PoseStack poseStack, SubmitNodeCollector submitNodeCollector, int packedLight) {
		ItemStackRenderState renderData = entityRenderState.getRenderData(BACKPACK);
		if (renderData == null) {
			return;
		}

		boolean wearsArmor = Boolean.TRUE.equals(entityRenderState.getRenderData(WEARS_ARMOR));
		boolean isBaby = entityRenderState.isBaby;
		EntityType<?> entityType = entityRenderState.getRenderData(ENTITY_TYPE);

		translateRotateAndScale(parentModel, entityType, isBaby, poseStack, wearsArmor);
		renderData.submit(poseStack, submitNodeCollector, packedLight, OverlayTexture.NO_OVERLAY, 0);
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
	}
}
