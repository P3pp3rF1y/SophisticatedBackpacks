package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.Minecraft;
import net.minecraft.client.model.geom.EntityModelSet;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.world.item.Item;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.ClientEventHandler.BACKPACK_LAYER;

public class BackpackModelManager {
	private static IBackpackModelProvider backpackModelProvider = new IBackpackModelProvider() {
		private static IBackpackModel model;

		@Override
		public void initModels() {
			if (model == null) {
				EntityModelSet entityModels = Minecraft.getInstance().getEntityModels();
				model = new BackpackModel(entityModels.bakeLayer(BACKPACK_LAYER));
			}
		}

		@Override
		public void initModels(BlockEntityRendererProvider.Context context) {
			if (model == null) {
				model = new BackpackModel(context.bakeLayer(BACKPACK_LAYER));
			}
		}

		@Override
		public IBackpackModel getBackpackModel(Item backpackItem) {
			return model;
		}
	};

	@SuppressWarnings("unused") // used by addon mod
	public static void registerBackpackModelProvider(IBackpackModelProvider provider) {
		backpackModelProvider = provider;
	}

	public static void initModels() {
		backpackModelProvider.initModels();
	}

	public static IBackpackModel getBackpackModel(Item backpackItem) {
		return backpackModelProvider.getBackpackModel(backpackItem);
	}
}
