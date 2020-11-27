package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;

public interface IItemHandlerInteractionUpgrade {
	void onHandlerInteract(IBackpackWrapper wrapper, IItemHandler itemHandler);
}
