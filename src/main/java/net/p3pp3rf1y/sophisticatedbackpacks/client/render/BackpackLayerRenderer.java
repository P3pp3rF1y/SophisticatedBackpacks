package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.client.renderer.item.ItemStackRenderState;
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
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;

import java.util.function.BiConsumer;

public class BackpackLayerRenderer<S extends LivingEntityRenderState, M extends EntityModel<S>> extends RenderLayer<S, M> {
	private static final ContextKey<ItemStack> BACKPACK_ITEM_STACK = new ContextKey<>(SophisticatedBackpacks.getRL("backpack_item_stack"));
	private static final ContextKey<Boolean> WEARS_ARMOR = new ContextKey<>(SophisticatedBackpacks.getRL("wears_armor"));
	private static final ContextKey<EntityType<?>> ENTITY_TYPE = new ContextKey<>(SophisticatedBackpacks.getRL("entity_type"));
	private static final ContextKey<ItemStackRenderState> DISPLAY_ITEM = new ContextKey<>(SophisticatedBackpacks.getRL("display_item"));
	private static final ContextKey<Integer> DISPLAY_ITEM_ROTATION = new ContextKey<>(SophisticatedBackpacks.getRL("display_item_rotation"));
	public static final BiConsumer<LivingEntity, LivingEntityRenderState> RENDER_STATE_MODIFIER = (livingEntity, entityRenderState) -> {
		if (livingEntity instanceof Player player) {
			PlayerInventoryProvider.get().getBackpackFromRendered(player, true).ifPresent(backpackRenderInfo -> {
				ItemStack backpack = backpackRenderInfo.getBackpack();
				entityRenderState.setRenderData(BACKPACK_ITEM_STACK, backpack);
				IBackpackModel model = BackpackModelManager.getBackpackModel(backpack.getItem());
				EquipmentSlot equipmentSlot = model.getRenderEquipmentSlot();
				entityRenderState.setRenderData(WEARS_ARMOR, (equipmentSlot != EquipmentSlot.CHEST || !backpackRenderInfo.isArmorSlot()) && !player.getItemBySlot(equipmentSlot).isEmpty());
				addDisplayItem(entityRenderState, backpack);
			});
		} else {
			ItemStack chestStack = livingEntity.getItemBySlot(EquipmentSlot.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				entityRenderState.setRenderData(BACKPACK_ITEM_STACK, livingEntity.getItemBySlot(EquipmentSlot.CHEST));
				entityRenderState.setRenderData(WEARS_ARMOR, false);
				addDisplayItem(entityRenderState, chestStack);
			}
		}
		entityRenderState.setRenderData(ENTITY_TYPE, livingEntity.getType());
	};

	private static void addDisplayItem(LivingEntityRenderState entityRenderState, ItemStack backpack) {
		RenderDataHandler renderDataHandler = BackpackWrapper.fromStack(backpack).getRenderDataHandler();
		RenderData.DisplayData displayData = renderDataHandler.getDisplayData();
		if (!displayData.displayItems().isEmpty()) {
			RenderData.DisplayItemData displayItem = displayData.displayItems().getFirst();
			entityRenderState.setRenderData(DISPLAY_ITEM_ROTATION, displayItem.rotation());
			ItemStackRenderState displayItemState = new ItemStackRenderState();
			Minecraft mc = Minecraft.getInstance();
			ItemModelResolver itemModelResolver = mc.getItemModelResolver();
			itemModelResolver.updateForTopItem(displayItemState, displayItem.item(), ItemDisplayContext.FIXED, mc.level, null, 0);
			entityRenderState.setRenderData(DISPLAY_ITEM, displayItemState);
		}
	}

	public BackpackLayerRenderer(RenderLayerParent<S, M> entityRendererIn) {
		super(entityRendererIn);
		BackpackModelManager.initModels();
	}

	@Override
	public void submit(PoseStack poseStack, SubmitNodeCollector submitNodeCollector, int packedLight, S entityRenderState, float netHeadYaw, float headPitch) {
		ItemStack backpack = entityRenderState.getRenderData(BACKPACK_ITEM_STACK);
		if (backpack == null) {
			return;
		}
		IBackpackModel model = BackpackModelManager.getBackpackModel(backpack.getItem());
		poseStack.pushPose();
		submitBackpack(getParentModel(), entityRenderState, poseStack, submitNodeCollector, packedLight, backpack, model);
		poseStack.popPose();
	}

	public static <S extends LivingEntityRenderState, M extends EntityModel<? super S>> void submitBackpack(M parentModel, S entityRenderState, PoseStack poseStack, SubmitNodeCollector submitNodeCollector, int packedLight, ItemStack backpack, IBackpackModel model) {
		boolean wearsArmor = Boolean.TRUE.equals(entityRenderState.getRenderData(WEARS_ARMOR));
		boolean isBaby = entityRenderState.isBaby;
		EntityType<?> entityType = entityRenderState.getRenderData(ENTITY_TYPE);

		model.translateRotateAndScale(parentModel, entityType, isBaby, poseStack, wearsArmor);

		IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
		int clothColor = wrapper.getMainColor();
		int borderColor = wrapper.getAccentColor();
		model.submit(poseStack, submitNodeCollector, packedLight, clothColor, borderColor, backpack.getItem(), wrapper.getRenderDataHandler());
		ItemStackRenderState displayItem = entityRenderState.getRenderData(DISPLAY_ITEM);
		if (displayItem != null) {
			submitItemShown(poseStack, submitNodeCollector, packedLight, entityRenderState.getRenderData(DISPLAY_ITEM_ROTATION), displayItem);
		}
	}

	private static void submitItemShown(PoseStack poseStack, SubmitNodeCollector submitNodeCollector, int packedLight, int rotation, ItemStackRenderState displayItem) {
		poseStack.pushPose();
		poseStack.translate(0, 0.9, -0.25);
		poseStack.scale(0.5f, 0.5f, 0.5f);
		poseStack.mulPose(Axis.ZP.rotationDegrees(180f + rotation));
		displayItem.submit(poseStack, submitNodeCollector, packedLight, OverlayTexture.NO_OVERLAY, 0);
		poseStack.popPose();
	}
}
