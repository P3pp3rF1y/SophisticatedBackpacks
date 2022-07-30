package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.world.item.Item;

public interface IBackpackModelProvider {
	void initModels();

	void initModels(BlockEntityRendererProvider.Context context);

	IBackpackModel getBackpackModel(Item backpackItem);
}
