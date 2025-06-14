package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import top.theillusivec4.curios.api.client.ICurioRenderer;

public class CuriosCompatClient {
	public static void registerRenderers() {
		ICurioRenderer.register(ModItems.BACKPACK.get(), BackpackCurioRenderer::new);
		ICurioRenderer.register(ModItems.COPPER_BACKPACK.get(), BackpackCurioRenderer::new);
		ICurioRenderer.register(ModItems.IRON_BACKPACK.get(), BackpackCurioRenderer::new);
		ICurioRenderer.register(ModItems.GOLD_BACKPACK.get(), BackpackCurioRenderer::new);
		ICurioRenderer.register(ModItems.DIAMOND_BACKPACK.get(), BackpackCurioRenderer::new);
		ICurioRenderer.register(ModItems.NETHERITE_BACKPACK.get(), BackpackCurioRenderer::new);
	}
}
