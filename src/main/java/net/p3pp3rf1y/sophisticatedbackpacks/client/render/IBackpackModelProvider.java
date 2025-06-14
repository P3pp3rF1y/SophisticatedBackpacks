package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.world.item.Item;

public interface IBackpackModelProvider {
	void initModels();

	IBackpackModel getBackpackModel(Item backpackItem);
}
