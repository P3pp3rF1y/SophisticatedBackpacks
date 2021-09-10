package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.vertex.IVertexBuilder;
import net.minecraft.client.entity.player.AbstractClientPlayerEntity;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.entity.IEntityRenderer;
import net.minecraft.client.renderer.entity.layers.LayerRenderer;
import net.minecraft.client.renderer.entity.model.BipedModel;
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
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.TankPosition;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class BackpackLayerRenderer<T extends LivingEntity, M extends BipedModel<T>> extends LayerRenderer<T, M> {
	private static final float CHILD_Y_OFFSET = 0.3F;
	private static final float CHILD_Z_OFFSET = 0.1F;
	private static final float CHILD_SCALE = 0.55F;
	private static final ResourceLocation TANK_GLASS_TEXTURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/entity/tank_glass.png");

	private static final BackpackModel MODEL = new BackpackModel();
	private static final TankGlassModel TANK_GLASS_MODEL = new TankGlassModel();

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
			SophisticatedBackpacks.PROXY.getPlayerInventoryProvider().getBackpackFromRendered(player).ifPresent(backpackRenderInfo -> {
				matrixStack.pushPose();
				boolean wearsArmor = !backpackRenderInfo.isArmorSlot() && !player.inventory.armor.get(EquipmentSlotType.CHEST.getIndex()).isEmpty();
				ItemStack backpack = backpackRenderInfo.getBackpack();
				renderBackpack(player, matrixStack, buffer, packedLight, backpack, wearsArmor);
				matrixStack.popPose();
			});
		} else {
			ItemStack chestStack = entity.getItemBySlot(EquipmentSlotType.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				renderBackpack(entity, matrixStack, buffer, packedLight, chestStack, false);
			}
		}
	}

	public static void renderBackpack(LivingEntity livingEntity, MatrixStack matrixStack, IRenderTypeBuffer buffer, int packedLight, ItemStack backpack, boolean wearsArmor) {
		if (livingEntity.isCrouching()) {
			matrixStack.translate(0D, 0.2D, 0D);
			matrixStack.mulPose(Vector3f.XP.rotationDegrees(90F / (float) Math.PI));
		}

		matrixStack.mulPose(Vector3f.YP.rotationDegrees(180));
		float zOffset = wearsArmor ? -0.35f : -0.3f;
		float yOffset = -0.75f;

		if (livingEntity.isBaby()) {
			zOffset += CHILD_Z_OFFSET;
			yOffset = CHILD_Y_OFFSET;
		}

		matrixStack.translate(0, yOffset, zOffset);

		if (livingEntity.isBaby()) {
			matrixStack.scale(CHILD_SCALE, CHILD_SCALE, CHILD_SCALE);
		}

		if (entityTranslations.containsKey(livingEntity.getType())) {
			Vector3d translVector = entityTranslations.get(livingEntity.getType());
			matrixStack.translate(translVector.x(), translVector.y(), translVector.z());
		}

		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
			IVertexBuilder vertexBuilder = buffer.getBuffer(RenderType.entityCutoutNoCull(RenderHelper.BACKPACK_ENTITY_TEXTURE));

			int clothColor = wrapper.getClothColor();
			int borderColor = wrapper.getBorderColor();
			Item backpackItem = backpack.getItem();

			BackpackRenderInfo renderInfo = wrapper.getRenderInfo();
			Set<TankPosition> tankPositions = renderInfo.getTankRenderInfos().keySet();
			boolean showLeftTank = tankPositions.contains(TankPosition.LEFT);
			boolean showRightTank = tankPositions.contains(TankPosition.RIGHT);
			Optional<IRenderedBatteryUpgrade.BatteryRenderInfo> batteryRenderInfo = renderInfo.getBatteryRenderInfo();
			MODEL.render(matrixStack, packedLight, vertexBuilder, clothColor, borderColor, backpackItem, showLeftTank, showRightTank, batteryRenderInfo.isPresent());

			renderFluids(matrixStack, buffer, packedLight, renderInfo, showLeftTank, showRightTank);
			batteryRenderInfo.ifPresent(info -> renderBatteryCharge(matrixStack, buffer, packedLight, info.getChargeRatio()));
		});
	}

	private static void renderBatteryCharge(MatrixStack matrixStack, IRenderTypeBuffer buffer, int packedLight, float chargeRatio) {
		matrixStack.pushPose();
		matrixStack.translate(0, 1.5, 0);
		RenderHelper.renderBatteryCharge(matrixStack, buffer, packedLight, chargeRatio);
		matrixStack.popPose();
	}

	private static void renderFluids(MatrixStack matrixStack, IRenderTypeBuffer buffer, int packedLight, BackpackRenderInfo renderInfo, boolean showLeftTank, boolean showRightTank) {
		IVertexBuilder vertexBuilder;
		matrixStack.pushPose();
		matrixStack.scale(1 / 2f, 6 / 10f, 1 / 2f);
		vertexBuilder = buffer.getBuffer(RenderType.entityCutoutNoCull(TANK_GLASS_TEXTURE));
		TANK_GLASS_MODEL.render(matrixStack, vertexBuilder, packedLight, showLeftTank, showRightTank);
		if (showLeftTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.LEFT);
			tankRenderInfo.getFluid().ifPresent(fluid -> RenderHelper.renderFluid(matrixStack, buffer, packedLight, fluid, tankRenderInfo.getFillRatio(), -14.5F, 37.5F, -1, -2F));
		}
		if (showRightTank) {
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.RIGHT);
			tankRenderInfo.getFluid().ifPresent(fluid -> RenderHelper.renderFluid(matrixStack, buffer, packedLight, fluid, tankRenderInfo.getFillRatio(), 11F, 37.5F, -1, -2F));
		}
		matrixStack.popPose();
	}

}