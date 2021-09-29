package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.world.entity.player.Player;
import net.minecraftforge.items.IItemHandler;

public interface IItemHandlerInteractionUpgrade {
	void onHandlerInteract(IItemHandler itemHandler, Player player);
}
