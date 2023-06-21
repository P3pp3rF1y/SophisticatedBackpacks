package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.player.AbstractClientPlayer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedcore.client.render.UpgradeRenderRegistry;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeRenderDataType;
import org.joml.Vector3f;

import javax.annotation.Nullable;

public class BackpackLayerRenderer<T extends LivingEntity, M extends EntityModel<T>> extends RenderLayer<T, M> {
	public BackpackLayerRenderer(RenderLayerParent<T, M> entityRendererIn) {
		super(entityRendererIn);
		BackpackModelManager.initModels();
	}

	@Override
	public void render(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, T entity, float limbSwing, float limbSwingAmount, float partialTicks, float ageInTicks, float netHeadYaw, float headPitch) {
		if (entity instanceof AbstractClientPlayer player) {
			PlayerInventoryProvider.get().getBackpackFromRendered(player).ifPresent(backpackRenderInfo -> {
				matrixStack.pushPose();
				ItemStack backpack = backpackRenderInfo.getBackpack();
				IBackpackModel model = BackpackModelManager.getBackpackModel(backpack.getItem());
				EquipmentSlot equipmentSlot = model.getRenderEquipmentSlot();
				boolean wearsArmor = (equipmentSlot != EquipmentSlot.CHEST || !backpackRenderInfo.isArmorSlot()) && !player.getInventory().armor.get(equipmentSlot.getIndex()).isEmpty();
				renderBackpack(getParentModel(), player, matrixStack, buffer, packedLight, backpack, wearsArmor, model);
				matrixStack.popPose();
			});
		} else {
			ItemStack chestStack = entity.getItemBySlot(EquipmentSlot.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				renderBackpack(getParentModel(), entity, matrixStack, buffer, packedLight, chestStack, false, BackpackModelManager.getBackpackModel(chestStack.getItem()));
			}
		}
	}

	public static <T extends LivingEntity, M extends EntityModel<T>> void renderBackpack(M parentModel, LivingEntity livingEntity, PoseStack matrixStack, MultiBufferSource buffer, int packedLight, ItemStack backpack, boolean wearsArmor, IBackpackModel model) {
		model.translateRotateAndScale(parentModel, livingEntity, matrixStack, wearsArmor);

		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
			int clothColor = wrapper.getMainColor();
			int borderColor = wrapper.getAccentColor();
			model.render(parentModel, livingEntity, matrixStack, buffer, packedLight, clothColor, borderColor, backpack.getItem(), wrapper.getRenderInfo());
			renderUpgrades(livingEntity, wrapper.getRenderInfo());
			renderItemShown(matrixStack, buffer, packedLight, wrapper.getRenderInfo(), livingEntity.level());
		});
	}

	private static void renderItemShown(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, RenderInfo renderInfo, @Nullable Level level) {
		renderInfo.getItemDisplayRenderInfo().getDisplayItem().ifPresent(displayItem -> {
			matrixStack.pushPose();
			matrixStack.translate(0, 0.9, -0.25);
			matrixStack.scale(0.5f, 0.5f, 0.5f);
			matrixStack.mulPose(Axis.ZP.rotationDegrees(180f + displayItem.getRotation()));
			Minecraft.getInstance().getItemRenderer().renderStatic(displayItem.getItem(), ItemDisplayContext.FIXED, packedLight, OverlayTexture.NO_OVERLAY, matrixStack, buffer, level, 0);
			matrixStack.popPose();
		});
	}

	private static void renderUpgrades(LivingEntity livingEntity, RenderInfo renderInfo) {
		if (Minecraft.getInstance().isPaused() || livingEntity.level().random.nextInt(32) != 0) {
			return;
		}
		renderInfo.getUpgradeRenderData().forEach((type, data) -> UpgradeRenderRegistry.getUpgradeRenderer(type).ifPresent(renderer -> renderUpgrade(renderer, livingEntity, type, data)));
	}

	private static Vector3f getBackpackMiddleFacePoint(LivingEntity livingEntity, Vector3f vector) {
		Vector3f point = new Vector3f(vector);
		point.rotate(Axis.XP.rotationDegrees(livingEntity.isCrouching() ? 25 : 0));
		point.add(0, 0.8f, livingEntity.isCrouching() ? 0.9f : 0.7f);
		point.rotate(Axis.YN.rotationDegrees(livingEntity.yBodyRot - 180));
		point.add(livingEntity.position().toVector3f());
		return point;
	}

	private static <T extends IUpgradeRenderData> void renderUpgrade(IUpgradeRenderer<T> renderer, LivingEntity livingEntity, UpgradeRenderDataType<?> type, IUpgradeRenderData data) {
		//noinspection unchecked
		type.cast(data).ifPresent(renderData -> renderer.render(livingEntity.level(), livingEntity.level().random, vector3d -> getBackpackMiddleFacePoint(livingEntity, vector3d), (T) renderData));
	}
}
