package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.world.entity.player.Player;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemResource;

public interface IItemResourceHandlerInteractionUpgrade {
	void onHandlerInteract(ResourceHandler<ItemResource> itemResourceHandler, Player player);
}
