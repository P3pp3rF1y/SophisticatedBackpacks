package net.p3pp3rf1y.sophisticatedbackpacks.compat.accessories;

import com.mojang.blaze3d.vertex.PoseStack;
import io.wispforest.accessories.api.AccessoriesStorageLookup;
import io.wispforest.accessories.api.client.AccessoryRenderState;
import io.wispforest.accessories.api.client.renderers.AccessoryRenderer;
import io.wispforest.accessories.api.slot.SlotPath;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

public class BackpackAccessoryRenderer implements AccessoryRenderer {
	@Override
	public <S extends LivingEntityRenderState> void render(AccessoryRenderState accessoryState, S entityState, EntityModel<S> model, PoseStack poseStack,
			SubmitNodeCollector collector) {
		// noop - this is rendered by BackpackLayerRenderer
	}

	@Override
	public void extractRenderState(ItemStack stack, SlotPath path, AccessoriesStorageLookup storageLookup, LivingEntity entity,
			LivingEntityRenderState entityState, AccessoryRenderState accessoryState) {
		AccessoryRenderer.super.extractRenderState(stack, path, storageLookup, entity, entityState, accessoryState);
		BackpackLayerRenderer.addBackpackRenderState(entityState, entity, new PlayerInventoryProvider.RenderInfo(stack, false));
	}
}
