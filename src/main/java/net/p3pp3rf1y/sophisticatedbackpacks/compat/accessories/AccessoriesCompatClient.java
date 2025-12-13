package net.p3pp3rf1y.sophisticatedbackpacks.compat.accessories;

import com.mojang.blaze3d.vertex.PoseStack;
import io.wispforest.accessories.api.client.AccessoriesRendererRegistry;
import io.wispforest.accessories.api.client.AccessoryRenderer;
import io.wispforest.accessories.api.slot.SlotReference;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.function.Supplier;

public class AccessoriesCompatClient {
	private static final AccessoryRenderer NO_RENDERER = new AccessoryRenderer() {
		@Override
		public <S extends LivingEntityRenderState> void render(ItemStack stack, SlotReference reference, PoseStack matrices, EntityModel<S> model, S renderState, MultiBufferSource multiBufferSource, int light, float partialTicks) {
			//noop
		}
	};
	private static final Supplier<AccessoryRenderer> NO_RENDERER_SUPPLIER = () -> NO_RENDERER;

	public static void registerRenderers() {
		ModItems.ITEMS.getEntries().forEach(holder -> {
			if (holder.get() instanceof BackpackItem) {
				AccessoriesRendererRegistry.registerRenderer(holder.get(), NO_RENDERER_SUPPLIER);
			}
		});
	}
}
