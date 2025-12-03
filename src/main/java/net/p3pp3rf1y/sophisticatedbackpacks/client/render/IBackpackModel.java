package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.item.Item;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import org.jetbrains.annotations.Nullable;

public interface IBackpackModel {
	void submit(PoseStack poseStack, SubmitNodeCollector submitNodeCollector, int packedLight, int clothColor, int borderColor, Item backpackItem, RenderDataHandler renderDataHandler);

	void submitBatteryCharge(SubmitNodeCollector submitNodeCollector, PoseStack poseStack, int packedLight, float chargeRatio);

	void submitFluid(SubmitNodeCollector submitNodeCollector, PoseStack poseStack, TextureAtlasSprite sprite, float fill, int color, boolean left, int packedLight);

	EquipmentSlot getRenderEquipmentSlot();

	<S extends EntityRenderState, M extends EntityModel<? super S>> void translateRotateAndScale(M parentModel, @Nullable EntityType<?> entityType, boolean isBaby, PoseStack poseStack, boolean wearsArmor);
}
