package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.player.AbstractClientPlayer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedcore.client.render.UpgradeRenderRegistry;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeRenderDataType;
import org.joml.Vector3f;

public class BackpackLayerRenderer<T extends LivingEntity, M extends EntityModel<T>> extends RenderLayer<T, M> {
	private static final float BABY_BODY_SCALE = 0.5F;
	private static final float BABY_BODY_Y_OFFSET = 1.5F;

	private static ItemRenderer itemRenderer;

	public BackpackLayerRenderer(RenderLayerParent<T, M> entityRendererIn) {
		super(entityRendererIn);
		itemRenderer = Minecraft.getInstance().getItemRenderer();
	}

	@Override
	public void render(PoseStack poseStack, MultiBufferSource buffer, int packedLight, T entity, float limbSwing, float limbSwingAmount, float partialTicks, float ageInTicks, float netHeadYaw, float headPitch) {
		if (entity instanceof AbstractClientPlayer player) {
			PlayerInventoryProvider.get().getBackpackFromRendered(player).ifPresent(backpackRenderInfo -> {
				poseStack.pushPose();
				ItemStack backpack = backpackRenderInfo.getBackpack();
				boolean wearsArmor = !backpackRenderInfo.isArmorSlot() && !player.getInventory().armor.get(EquipmentSlot.CHEST.getIndex()).isEmpty();
				renderBackpack(getParentModel(), player, poseStack, buffer, packedLight, backpack, wearsArmor);
				poseStack.popPose();
			});
		} else {
			poseStack.pushPose();
			ItemStack chestStack = entity.getItemBySlot(EquipmentSlot.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				renderBackpack(getParentModel(), entity, poseStack, buffer, packedLight, chestStack, false);
			}
			poseStack.popPose();
		}
	}

	public static <T extends LivingEntity, M extends EntityModel<T>> void renderBackpack(M parentModel, LivingEntity livingEntity, PoseStack poseStack, MultiBufferSource buffer, int packedLight, ItemStack backpack, boolean wearsArmor) {
		translateRotateAndScale(parentModel, livingEntity, poseStack, wearsArmor);
		IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
		itemRenderer.renderStatic(backpack, BackpackDynamicModel.WORN, packedLight, OverlayTexture.NO_OVERLAY, poseStack, buffer, livingEntity.level(), 0);
		renderUpgrades(livingEntity, wrapper.getRenderInfo());
	}

	private static <L extends LivingEntity, M extends EntityModel<L>> void translateRotateAndScale(M parentModel, LivingEntity livingEntity, PoseStack poseStack, boolean wearsArmor) {
		if (parentModel instanceof HumanoidModel<?> humanoidModel) {
			if (livingEntity.isBaby() && !(livingEntity instanceof Player)) {
				poseStack.scale(BABY_BODY_SCALE, BABY_BODY_SCALE, BABY_BODY_SCALE);
				poseStack.translate(0.0F, BABY_BODY_Y_OFFSET, 0.0F);
			}

			humanoidModel.body.translateAndRotate(poseStack);
		}

		poseStack.mulPose(Axis.YP.rotationDegrees(180));
		poseStack.mulPose(Axis.ZP.rotationDegrees(180));
		float zOffset = wearsArmor ? -0.35f : -0.3f;
		float yOffset = -0.25f;

		poseStack.translate(0, yOffset, zOffset);

		if (livingEntity instanceof Player) {
			return;
		}
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
