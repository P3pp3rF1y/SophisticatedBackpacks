package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.math.Vector3f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.AgeableListModel;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.PartPose;
import net.minecraft.client.model.geom.builders.*;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.item.Item;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IRenderedTankUpgrade;

import javax.annotation.Nonnull;
import java.util.*;

public class BackpackModel extends AgeableListModel<LivingEntity> implements IBackpackModel {
	private static final Map<EntityType<?>, Vec3> entityTranslations;

	static {
		entityTranslations = new HashMap<>();
		entityTranslations.put(EntityType.ENDERMAN, new Vec3(0, -0.8, 0));
	}

	private static final ResourceLocation BACKPACK_ENTITY_TEXTURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/entity/backpack.png");
	private static final ResourceLocation TANK_GLASS_TEXTURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/entity/tank_glass.png");
	public static final float CHILD_Y_OFFSET = 0.3F;
	public static final float CHILD_Z_OFFSET = 0.1F;
	public static final float CHILD_SCALE = 0.55F;

	private static final String CLOTH_PART = "cloth";
	private static final String RIGHT_POUCHES_BORDER_PART = "rightPouchesBorder";
	private static final String LEFT_POUCHES_BORDER_PART = "leftPouchesBorder";
	private static final String FRONT_POUCH_BORDER_PART = "frontPouchBorder";
	private static final String FRONT_POUCH_PART = "frontPouch";
	private static final String RIGHT_POUCHES_PART = "rightPouches";
	private static final String LEFT_POUCHES_PART = "leftPouches";
	private static final String BORDER_PART = "border";
	private static final String FABRIC_FRONT_PART = "fabricFront";
	private static final String FABRIC_RIGHT_PART = "fabricRight";
	private static final String FABRIC_LEFT_PART = "fabricLeft";
	private static final String FABRIC_PART = "fabric";
	private static final String BATTERY_BORDER_PART = "batteryBorder";
	private static final String BATTERY_PART = "battery";
	private static final String LEFT_TANK_BORDER_PART = "leftTankBorder";
	private static final String LEFT_TANK_PART = "leftTank";
	private static final String RIGHT_TANK_BORDER_PART = "rightTankBorder";
	private static final String RIGHT_TANK_PART = "rightTank";
	private static final String BODY_CLIPS_PART = "bodyClips_";
	private static final String LEFT_POUCHES_CLIPS_PART = "leftPouchesClips_";
	private static final String RIGHT_POUCHES_CLIPS_PART = "rightPouchesClips_";
	private static final String FRONT_POUCH_CLIPS_PART = "frontPouchClips_";
	private static final String BATTERY_CLIPS_PART = "batteryClips_";
	private static final String LEFT_TANK_GLASS_PART = "leftTankGlass";
	private static final String RIGHT_TANK_GLASS_PART = "rightTankGlass";
	private static final String BATTERY_CHARGE_PART = "battery_charge_";
	private final Map<Item, ModelPart> clipsBody;
	private final Map<Item, ModelPart> clipsLeftPouches;
	private final Map<Item, ModelPart> clipsRightPouches;
	private final Map<Item, ModelPart> clipsFrontPouch;
	private final Map<Item, ModelPart> clipsBattery;
	private final Map<FluidBarCacheKey, ModelPart> fluidLevelsLeft = new HashMap<>();
	private final Map<FluidBarCacheKey, ModelPart> fluidLevelsRight = new HashMap<>();
	private final Map<Integer, ModelPart> batteryCharges;

	public final ModelPart cloth;
	private final ModelPart rightPouchesBorder;
	private final ModelPart leftPouchesBorder;
	private final ModelPart frontPouchBorder;
	private final ModelPart frontPouch;
	private final ModelPart rightPouches;
	private final ModelPart leftPouches;
	public final ModelPart border;
	private final ModelPart fabricFront;
	private final ModelPart fabricRight;
	private final ModelPart fabricLeft;
	public final ModelPart fabric;
	private final ModelPart battery;
	private final ModelPart batteryBorder;
	private final ModelPart leftTank;
	private final ModelPart leftTankBorder;
	private final ModelPart rightTank;
	private final ModelPart rightTankBorder;
	public final ModelPart leftTankGlass;
	public final ModelPart rightTankGlass;

	public BackpackModel(ModelPart part) {
		cloth = part.getChild(CLOTH_PART);
		rightPouchesBorder = part.getChild(RIGHT_POUCHES_BORDER_PART);
		leftPouchesBorder = part.getChild(LEFT_POUCHES_BORDER_PART);
		frontPouchBorder = part.getChild(FRONT_POUCH_BORDER_PART);
		frontPouch = part.getChild(FRONT_POUCH_PART);
		rightPouches = part.getChild(RIGHT_POUCHES_PART);
		leftPouches = part.getChild(LEFT_POUCHES_PART);
		border = part.getChild(BORDER_PART);
		fabricFront = part.getChild(FABRIC_FRONT_PART);
		fabricRight = part.getChild(FABRIC_RIGHT_PART);
		fabricLeft = part.getChild(FABRIC_LEFT_PART);
		fabric = part.getChild(FABRIC_PART);
		battery = part.getChild(BATTERY_PART);
		batteryBorder = part.getChild(BATTERY_BORDER_PART);
		leftTank = part.getChild(LEFT_TANK_PART);
		leftTankBorder = part.getChild(LEFT_TANK_BORDER_PART);
		rightTank = part.getChild(RIGHT_TANK_PART);
		rightTankBorder = part.getChild(RIGHT_TANK_BORDER_PART);

		ImmutableMap.Builder<Item, ModelPart> clipsBodyBuilder = ImmutableMap.builder();
		ImmutableMap.Builder<Item, ModelPart> clipsLeftPouchesBuilder = ImmutableMap.builder();
		ImmutableMap.Builder<Item, ModelPart> clipsRightPouchesBuilder = ImmutableMap.builder();
		ImmutableMap.Builder<Item, ModelPart> clipsFrontPouchBuilder = ImmutableMap.builder();
		ImmutableMap.Builder<Item, ModelPart> clipsBatteryBuilder = ImmutableMap.builder();

		getBackpackItems().values().forEach(backpackItem -> {
			clipsBodyBuilder.put(backpackItem, part.getChild(getTierPartName(backpackItem, BODY_CLIPS_PART)));
			clipsLeftPouchesBuilder.put(backpackItem, part.getChild(getTierPartName(backpackItem, LEFT_POUCHES_CLIPS_PART)));
			clipsRightPouchesBuilder.put(backpackItem, part.getChild(getTierPartName(backpackItem, RIGHT_POUCHES_CLIPS_PART)));
			clipsFrontPouchBuilder.put(backpackItem, part.getChild(getTierPartName(backpackItem, FRONT_POUCH_CLIPS_PART)));
			clipsBatteryBuilder.put(backpackItem, part.getChild(getTierPartName(backpackItem, BATTERY_CLIPS_PART)));
		});

		ImmutableMap.Builder<Integer, ModelPart> batteryChargeBuilder = ImmutableMap.builder();
		for (int pixels = 1; pixels < 5; pixels++) {
			batteryChargeBuilder.put(pixels, part.getChild(BATTERY_CHARGE_PART + pixels));
		}
		batteryCharges = batteryChargeBuilder.build();

		clipsBody = clipsBodyBuilder.build();
		clipsLeftPouches = clipsLeftPouchesBuilder.build();
		clipsRightPouches = clipsRightPouchesBuilder.build();
		clipsFrontPouch = clipsFrontPouchBuilder.build();
		clipsBattery = clipsBatteryBuilder.build();

		ModelPart modelPart = getGlassModelPart();
		leftTankGlass = modelPart.getChild(LEFT_TANK_GLASS_PART);
		rightTankGlass = modelPart.getChild(RIGHT_TANK_GLASS_PART);
	}

	@Nonnull
	private ModelPart getGlassModelPart() {
		MeshDefinition meshdefinition = new MeshDefinition();
		PartDefinition partDefinition = meshdefinition.getRoot();
		partDefinition.addOrReplaceChild(LEFT_TANK_GLASS_PART, CubeListBuilder.create()
						.texOffs(18, 5).addBox(-15F, 3.5F, -2.5F, 4.0F, 10.0F, 0.0F)
						.texOffs(0, 0).addBox(-15F, 3.5F, -2.5F, 0.0F, 10.0F, 5.0F)
						.texOffs(10, 5).addBox(-15F, 3.5F, 2.5F, 4.0F, 10.0F, 0.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(RIGHT_TANK_GLASS_PART, CubeListBuilder.create()
						.texOffs(18, 5).addBox(11F, 3.5F, -2.5F, 4.0F, 10.0F, 0.0F, true)
						.texOffs(0, 0).addBox(15F, 3.5F, -2.5F, 0.0F, 10.0F, 5.0F, true)
						.texOffs(10, 5).addBox(11F, 3.5F, 2.5F, 4.0F, 10.0F, 0.0F, true)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		return partDefinition.bake(32, 32);
	}

	public static LayerDefinition createBodyLayer() {
		MeshDefinition meshdefinition = new MeshDefinition();
		PartDefinition partDefinition = meshdefinition.getRoot();
		partDefinition.addOrReplaceChild(CLOTH_PART, CubeListBuilder.create()
						.texOffs(0, 0)
						.addBox(-3.5F, -13.25F, -3.25F, 7.0F, 4.0F, 6.0F)
						.texOffs(0, 10)
						.addBox(-5.0F, -13.0F, -3.0F, 10.0F, 13.0F, 6.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);

		partDefinition.addOrReplaceChild(RIGHT_POUCHES_BORDER_PART, CubeListBuilder.create()
						.texOffs(44, 0).addBox(5.0F, -2.0F, -2.5F, 2.0F, 1.0F, 5.0F, true)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(LEFT_POUCHES_BORDER_PART, CubeListBuilder.create()
						.texOffs(44, 0).addBox(-7.0F, -2.0F, -2.5F, 2.0F, 1.0F, 5.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(FRONT_POUCH_BORDER_PART, CubeListBuilder.create()
						.texOffs(44, 0).addBox(-4.0F, -2.0F, -5.0F, 8.0F, 1.0F, 2.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(FRONT_POUCH_PART, CubeListBuilder.create()
						.texOffs(25, 0).addBox(-4.0F, -1.0F, -5.0F, 8.0F, 1.0F, 2.0F)
						.texOffs(13, 2).addBox(-4.0F, -4.0F, -5.0F, 8.0F, 2.0F, 2.0F)
						.texOffs(13, 0).addBox(-4.0F, -6.0F, -5.0F, 8.0F, 1.0F, 2.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(RIGHT_POUCHES_PART, CubeListBuilder.create()
						.texOffs(32, 5).addBox(5.0F, -1.0F, -2.5F, 2.0F, 1.0F, 5.0F)
						.texOffs(32, 13).addBox(5.0F, -4.0F, -2.5F, 2.0F, 2.0F, 5.0F)
						.texOffs(32, 11).addBox(5.0F, -6.0F, -2.5F, 2.0F, 1.0F, 5.0F)
						.texOffs(32, 22).addBox(5.0F, -9.0F, -2.5F, 1.0F, 2.0F, 5.0F)
						.texOffs(32, 20).addBox(5.0F, -11.0F, -2.5F, 1.0F, 1.0F, 5.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(LEFT_POUCHES_PART, CubeListBuilder.create()
						.texOffs(32, 5).addBox(-7.0F, -1.0F, -2.5F, 2.0F, 1.0F, 5.0F, true)
						.texOffs(32, 13).addBox(-7.0F, -4.0F, -2.5F, 2.0F, 2.0F, 5.0F, true)
						.texOffs(32, 11).addBox(-7.0F, -6.0F, -2.5F, 2.0F, 1.0F, 5.0F, true)
						.texOffs(32, 22).addBox(-6.0F, -9.0F, -2.5F, 1.0F, 2.0F, 5.0F, true)
						.texOffs(32, 20).addBox(-6.0F, -11.0F, -2.5F, 1.0F, 1.0F, 5.0F, true)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);

		partDefinition.addOrReplaceChild(BORDER_PART, CubeListBuilder.create()
						.texOffs(44, 7).addBox(-3.5F, -9.25F, -3.25F, 7.0F, 1.0F, 1.0F)
						.texOffs(50, 20).addBox(3.5F, -13.25F, -3.25F, 1.0F, 5.0F, 6.0F)
						.texOffs(50, 9).addBox(-4.5F, -13.25F, -3.25F, 1.0F, 5.0F, 6.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);

		partDefinition.addOrReplaceChild(FABRIC_FRONT_PART, CubeListBuilder.create()
						.texOffs(12, 58).addBox(-0.75F, 3.0F, 1.0F, 8.0F, 1.0F, 2.0F, true)
				, PartPose.offset(-3.25F, 16.0F, -6.0F)
		);
		partDefinition.addOrReplaceChild(FABRIC_RIGHT_PART, CubeListBuilder.create()
						.texOffs(32, 49).addBox(8.25F, -2.0F, 3.5F, 1.0F, 1.0F, 5.0F, true)
						.texOffs(8, 48).addBox(8.25F, 3.0F, 3.5F, 2.0F, 1.0F, 5.0F, true)
				, PartPose.offset(-3.25F, 16.0F, -6.0F)
		);
		partDefinition.addOrReplaceChild(FABRIC_LEFT_PART, CubeListBuilder.create()
						.texOffs(32, 49).addBox(-2.75F, -2.0F, 3.5F, 1.0F, 1.0F, 5.0F)
						.texOffs(8, 48).addBox(-3.75F, 3.0F, 3.5F, 2.0F, 1.0F, 5.0F)
				, PartPose.offset(-3.25F, 16.0F, -6.0F)
		);
		partDefinition.addOrReplaceChild(FABRIC_PART, CubeListBuilder.create()
						.texOffs(54, 0).addBox(1.25F, -4.75F, 5.75F, 1.0F, 1.0F, 1.0F)
						.texOffs(58, 0).addBox(4.25F, -4.75F, 5.75F, 1.0F, 1.0F, 1.0F)
						.texOffs(44, 0).addBox(1.25F, -5.75F, 5.75F, 4.0F, 1.0F, 1.0F, true)
						.texOffs(16, 47).addBox(0.0F, -5.5F, 2.5F, 1.0F, 4.0F, 7.0F)
						.texOffs(0, 47).addBox(5.5F, -5.5F, 2.5F, 1.0F, 4.0F, 7.0F)
				, PartPose.offset(-3.25F, 16.0F, -6.0F)
		);

		partDefinition.addOrReplaceChild(BATTERY_BORDER_PART, CubeListBuilder.create()
						.texOffs(28, 38).addBox(-4.25F, -5.25F, -6.25F, 1.0F, 1.0F, 4.0F)
						.texOffs(28, 43).addBox(-3.5F, -5.25F, -6.25F, 7.0F, 1.0F, 1.0F)
						.texOffs(33, 38).addBox(-4.25F, -1.25F, -6.25F, 1.0F, 1.0F, 4.0F)
						.texOffs(33, 38).addBox(3.25F, -5.25F, -6.25F, 1.0F, 1.0F, 4.0F)
						.texOffs(27, 45).addBox(-3.5F, -1.25F, -6.25F, 7.0F, 1.0F, 1.0F)
						.texOffs(39, 37).addBox(3.25F, -1.25F, -6.25F, 1.0F, 1.0F, 4.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);

		partDefinition.addOrReplaceChild(BATTERY_PART, CubeListBuilder.create()
						.texOffs(28, 29).addBox(-4.0F, -6.0F, -6.0F, 8.0F, 6.0F, 3.0F)
						.texOffs(28, 53).addBox(-2.0F, -6.25F, -4.5F, 1.0F, 1.0F, 1.0F)
						.texOffs(28, 53).addBox(-0.75F, -6.25F, -4.5F, 1.0F, 1.0F, 1.0F)
						.texOffs(28, 53).addBox(-2.0F, -8.0F, -3.25F, 1.0F, 1.0F, 1.0F)
						.texOffs(28, 53).addBox(-0.75F, -8.0F, -3.25F, 1.0F, 1.0F, 1.0F)
						.texOffs(0, 58).addBox(-2.0F, -7.4F, -4.5F, 1.0F, 2.0F, 1.0F, new CubeDeformation(-0.2F))
						.texOffs(6, 58).addBox(-0.75F, -7.4F, -4.5F, 1.0F, 2.0F, 1.0F, new CubeDeformation(-0.2F))
						.texOffs(0, 61).addBox(-2.0F, -8.0F, -4.5F, 1.0F, 1.0F, 2.0F, new CubeDeformation(-0.2F))
						.texOffs(6, 61).addBox(-0.75F, -8.0F, -4.5F, 1.0F, 1.0F, 2.0F, new CubeDeformation(-0.2F))
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(LEFT_TANK_BORDER_PART, CubeListBuilder.create()
						.texOffs(50, 43).addBox(-8.0F, -9.5F, -2.0F, 3.0F, 1.0F, 4.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(LEFT_TANK_PART, CubeListBuilder.create()
						.texOffs(54, 27).addBox(-5.5F, -7.5F, -2.0F, 1.0F, 6.0F, 4.0F)
						.texOffs(50, 37).addBox(-8.0F, -1.5F, -2.0F, 3.0F, 1.0F, 4.0F)
						.texOffs(50, 42).addBox(-8.0F, -8.5F, -2.0F, 3.0F, 1.0F, 4.0F)
						.texOffs(50, 37).addBox(-8.0F, -10.5F, -2.0F, 3.0F, 1.0F, 4.0F)
						.texOffs(52, 48).addBox(-7.5F, -11.5F, -1.5F, 3.0F, 1.0F, 3.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(RIGHT_TANK_BORDER_PART, CubeListBuilder.create()
						.texOffs(50, 43).addBox(5.0F, -9.5F, -2.0F, 3.0F, 1.0F, 4.0F, true)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
		partDefinition.addOrReplaceChild(RIGHT_TANK_PART, CubeListBuilder.create()
						.texOffs(54, 27).addBox(4.5F, -7.5F, -2.0F, 1.0F, 6.0F, 4.0F, true)
						.texOffs(50, 37).addBox(5.0F, -1.5F, -2.0F, 3.0F, 1.0F, 4.0F, true)
						.texOffs(50, 42).addBox(5.0F, -8.5F, -2.0F, 3.0F, 1.0F, 4.0F, true)
						.texOffs(50, 37).addBox(5.0F, -10.5F, -2.0F, 3.0F, 1.0F, 4.0F, true)
						.texOffs(52, 48).addBox(4.5F, -11.5F, -1.5F, 3.0F, 1.0F, 3.0F, true)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);

		for (int pixels = 1; pixels < 5; pixels++) {
			partDefinition.addOrReplaceChild(BATTERY_CHARGE_PART + pixels, CubeListBuilder.create()
							.texOffs(30, 58).addBox(-2.0F, 21F, -6.01F, pixels, 1.0F, 1.0F)
					, PartPose.ZERO
			);
		}
		getBackpackItems().forEach((idx, item) -> {
			addBodyClips(partDefinition, item, 29 + idx * 3);
			addLeftPouchesClips(partDefinition, item, 29 + idx * 3);
			addRightPouchesClips(partDefinition, item, 29 + idx * 3);
			addFrontPouchClips(partDefinition, item, 29 + idx * 3);
			addBatteryClips(partDefinition, item, 30 + idx * 3);
		});
		return LayerDefinition.create(meshdefinition, 64, 64);
	}

	private static Map<Integer, Item> getBackpackItems() {
		return new LinkedHashMap<>(Map.of(
				0, ModItems.BACKPACK.get(),
				1, ModItems.COPPER_BACKPACK.get(),
				2, ModItems.IRON_BACKPACK.get(),
				3, ModItems.GOLD_BACKPACK.get(),
				4, ModItems.DIAMOND_BACKPACK.get(),
				5, ModItems.NETHERITE_BACKPACK.get()
		));
	}

	@Override
	public <L extends LivingEntity, M extends EntityModel<L>> void render(M parentModel, LivingEntity livingEntity, PoseStack poseStack, MultiBufferSource buffer, int packedLight, int clothColor, int borderColor, Item backpackItem, RenderInfo renderInfo) {
		VertexConsumer vertexBuilder = buffer.getBuffer(RenderType.entityCutoutNoCull(BACKPACK_ENTITY_TEXTURE));
		Set<TankPosition> tankPositions = renderInfo.getTankRenderInfos().keySet();
		boolean showLeftTank = tankPositions.contains(TankPosition.LEFT);
		boolean showRightTank = tankPositions.contains(TankPosition.RIGHT);
		Optional<IRenderedBatteryUpgrade.BatteryRenderInfo> batteryRenderInfo = renderInfo.getBatteryRenderInfo();

		float borderRed = (borderColor >> 16 & 255) / 255.0F;
		float borderGreen = (borderColor >> 8 & 255) / 255.0F;
		float borderBlue = (borderColor & 255) / 255.0F;
		float clothRed = (clothColor >> 16 & 255) / 255.0F;
		float clothGreen = (clothColor >> 8 & 255) / 255.0F;
		float clothBlue = (clothColor & 255) / 255.0F;

		if (showLeftTank) {
			leftTank.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			leftTankBorder.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		} else {
			fabricLeft.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			clipsLeftPouches.get(backpackItem).render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			leftPouches.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);
			leftPouchesBorder.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		}

		if (showRightTank) {
			rightTank.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			rightTankBorder.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		} else {
			fabricRight.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			clipsRightPouches.get(backpackItem).render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			rightPouches.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);
			rightPouchesBorder.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		}

		if (batteryRenderInfo.isPresent()) {
			battery.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			batteryBorder.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
			clipsBattery.get(backpackItem).render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			batteryRenderInfo.ifPresent(info -> renderBatteryCharge(poseStack, buffer, packedLight, info.getChargeRatio()));
		} else {
			fabricFront.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			clipsFrontPouch.get(backpackItem).render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			frontPouch.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);
			frontPouchBorder.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);
		}

		fabric.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
		clipsBody.get(backpackItem).render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);

		cloth.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, clothRed, clothGreen, clothBlue, 1);

		border.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, borderRed, borderGreen, borderBlue, 1);

		poseStack.pushPose();
		poseStack.scale(1 / 2f, 6 / 10f, 1 / 2f);
		if (showLeftTank) {
			vertexBuilder = buffer.getBuffer(RenderType.entityCutoutNoCull(TANK_GLASS_TEXTURE));
			leftTankGlass.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.LEFT);
			tankRenderInfo.getFluid().ifPresent(f -> renderFluid(poseStack, buffer, packedLight, f, tankRenderInfo.getFillRatio(), true));
		}
		if (showRightTank) {
			vertexBuilder = buffer.getBuffer(RenderType.entityCutoutNoCull(TANK_GLASS_TEXTURE));
			rightTankGlass.render(poseStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
			IRenderedTankUpgrade.TankRenderInfo tankRenderInfo = renderInfo.getTankRenderInfos().get(TankPosition.RIGHT);
			tankRenderInfo.getFluid().ifPresent(f -> renderFluid(poseStack, buffer, packedLight, f, tankRenderInfo.getFillRatio(), false));
		}
		poseStack.popPose();
	}

	@Override
	public void renderBatteryCharge(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, float chargeRatio) {
		ModelPart charge = batteryCharges.get((int) (chargeRatio * 4));
		if (charge == null) {
			return;
		}
		VertexConsumer vertexBuilder = buffer.getBuffer(RenderType.entityTranslucent(BACKPACK_ENTITY_TEXTURE));
		charge.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY);
	}

	@Override
	public void renderFluid(PoseStack matrixStack, MultiBufferSource buffer, int packedLight, FluidStack fluidStack, float fill, boolean left) {
		if (Mth.equal(fill, 0.0f)) {
			return;
		}

		ResourceLocation texture = fluidStack.getFluid().getAttributes().getStillTexture(fluidStack);
		TextureAtlasSprite still = Minecraft.getInstance().getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(texture);
		VertexConsumer vertexBuilder = buffer.getBuffer(RenderType.entityTranslucent(InventoryMenu.BLOCK_ATLAS));
		ModelPart fluidBox = getFluidBar(still, (int) (fill * 10), left);
		int color = fluidStack.getFluid().getAttributes().getColor(fluidStack);
		float red = (color >> 16 & 255) / 255.0F;
		float green = (color >> 8 & 255) / 255.0F;
		float blue = (color & 255) / 255.0F;
		fluidBox.render(matrixStack, vertexBuilder, packedLight, OverlayTexture.NO_OVERLAY, red, green, blue, 1);
	}

	private ModelPart getFluidBar(TextureAtlasSprite still, int fill, boolean left) {
		int atlasWidth = (int) (still.getWidth() / (still.getU1() - still.getU0()));
		int atlasHeight = (int) (still.getHeight() / (still.getV1() - still.getV0()));
		int u = (int) (still.getU0() * atlasWidth);
		int v = (int) (still.getV0() * atlasHeight);
		FluidBarCacheKey key = new FluidBarCacheKey(u, v, fill);

		Map<FluidBarCacheKey, ModelPart> fluidLevels = left ? fluidLevelsLeft : fluidLevelsRight;
		return fluidLevels.computeIfAbsent(key, k -> {
			MeshDefinition meshdefinition = new MeshDefinition();
			PartDefinition partDefinition = meshdefinition.getRoot();
			partDefinition.addOrReplaceChild("fluid_fill", CubeListBuilder.create()
							.texOffs(u, v)
							.addBox(left ? -14.5F : 11F, 13.5F - fill, -2F, 3.5F, fill, 4F, !left)
					, PartPose.offset(0.0F, 24.0F, 0.0F));
			ModelPart root = partDefinition.bake(atlasWidth, atlasHeight);
			return root.getChild("fluid_fill");
		});
	}

	@Override
	public EquipmentSlot getRenderEquipmentSlot() {
		return EquipmentSlot.CHEST;
	}

	@Override
	public <L extends LivingEntity, M extends EntityModel<L>> void translateRotateAndScale(M parentModel, LivingEntity livingEntity, PoseStack matrixStack, boolean wearsArmor) {
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

		if (livingEntity instanceof Player) {
			return;
		}

		if (livingEntity.isBaby()) {
			matrixStack.scale(CHILD_SCALE, CHILD_SCALE, CHILD_SCALE);
		}

		if (entityTranslations.containsKey(livingEntity.getType())) {
			Vec3 translVector = entityTranslations.get(livingEntity.getType());
			matrixStack.translate(translVector.x(), translVector.y(), translVector.z());
		}
	}

	private record FluidBarCacheKey(int u, int v, int fill) {
		@Override
		public boolean equals(Object o) {
			if (this == o) {return true;}
			if (o == null || getClass() != o.getClass()) {return false;}
			FluidBarCacheKey that = (FluidBarCacheKey) o;
			return u == that.u && v == that.v && fill == that.fill;
		}

		@Override
		public int hashCode() {
			return Objects.hash(u, v, fill);
		}
	}

	private static void addBodyClips(PartDefinition partDefinition, Item backpackItem, int yTextureOffset) {
		addBodyClips(partDefinition, backpackItem, 0, yTextureOffset);
	}

	private static void addBodyClips(PartDefinition partDefinition, Item backpackItem, int xTextureOffset, int yTextureOffset) {
		partDefinition.addOrReplaceChild(getTierPartName(backpackItem, BODY_CLIPS_PART), CubeListBuilder.create()
						.texOffs(22 + xTextureOffset, yTextureOffset).addBox(-3.25F, -9.5F, -3.5F, 1.0F, 2.0F, 1.0F)
						.texOffs(25 + xTextureOffset, yTextureOffset).addBox(2.25F, -9.5F, -3.5F, 1.0F, 2.0F, 1.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
	}

	private static String getTierPartName(Item backpackItem, String partNamePrefix) {
		//noinspection ConstantConditions - by this point backpack items are registered
		return partNamePrefix + backpackItem.getRegistryName().getPath();
	}

	private static void addLeftPouchesClips(PartDefinition partDefinition, Item backpackItem, int yTextureOffset) {
		partDefinition.addOrReplaceChild(getTierPartName(backpackItem, LEFT_POUCHES_CLIPS_PART), CubeListBuilder.create()
						.texOffs(18, yTextureOffset).addBox(-6.25F, -10.0F, -0.5F, 1.0F, 2.0F, 1.0F)
						.texOffs(6, yTextureOffset).addBox(-7.25F, -5.0F, -0.5F, 1.0F, 2.0F, 1.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
	}

	private static void addRightPouchesClips(PartDefinition partDefinition, Item backpackItem, int yTextureOffset) {
		partDefinition.addOrReplaceChild(getTierPartName(backpackItem, RIGHT_POUCHES_CLIPS_PART), CubeListBuilder.create()
						.texOffs(6, yTextureOffset).addBox(6.25F, -5.0F, -0.5F, 1.0F, 2.0F, 1.0F, true)
						.texOffs(18, yTextureOffset).addBox(5.25F, -10.0F, -0.5F, 1.0F, 2.0F, 1.0F, true)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
	}

	private static void addFrontPouchClips(PartDefinition partDefinition, Item backpackItem, int yTextureOffset) {
		partDefinition.addOrReplaceChild(getTierPartName(backpackItem, FRONT_POUCH_CLIPS_PART), CubeListBuilder.create()
						.texOffs(0, yTextureOffset).addBox(2.0F, -5.0F, -5.25F, 1.0F, 2.0F, 1.0F)
						.texOffs(3, yTextureOffset).addBox(-3.0F, -5.0F, -5.25F, 1.0F, 2.0F, 1.0F)
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
	}

	private static void addBatteryClips(PartDefinition partDefinition, Item backpackItem, int yTextureOffset) {
		partDefinition.addOrReplaceChild(getTierPartName(backpackItem, BATTERY_CLIPS_PART), CubeListBuilder.create()
						.texOffs(24, yTextureOffset).addBox(1.0F, -5.25F, -6.15F, 1.0F, 1.0F, 1.0F, new CubeDeformation(0.2F))
						.texOffs(21, yTextureOffset).addBox(1.0F, -1.25F, -6.15F, 1.0F, 1.0F, 1.0F, new CubeDeformation(0.2F))
				, PartPose.offset(0.0F, 24.0F, 0.0F)
		);
	}

	@Override
	public void renderToBuffer(PoseStack matrixStack, VertexConsumer buffer, int packedLight, int packedOverlay, float red, float green, float blue, float alpha) {
		//noop
	}

	@Override
	protected Iterable<ModelPart> headParts() {
		return Collections.emptyList();
	}

	@Override
	protected Iterable<ModelPart> bodyParts() {
		return Collections.emptyList();
	}

	@Override
	public void setupAnim(LivingEntity entityIn, float limbSwing, float limbSwingAmount, float ageInTicks, float netHeadYaw, float headPitch) {
		//noop
	}
}